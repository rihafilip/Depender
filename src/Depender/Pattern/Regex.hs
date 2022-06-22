{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Depender.Pattern.Regex (regexPattern) where

import Control.Applicative (Alternative (many, some), Applicative (liftA2), (<|>))
import Control.Monad ((>=>))
import qualified Data.Bifunctor as Bf
import Data.Char (isSpace)
import Data.Functor (($>))
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Depender.DependencyGraph
import Depender.Pattern hiding (fromConfig, getPattern)
import Text.Parser
import Prelude hiding (any)

-- | Different types of regular expressions that are possible
data InnerRegex
  = Character Char
  | Output
  | AnyNonSpace
  | Any
  | StartOfLine
  | EndOfLine
  | Optional InnerRegex
  | ZeroOrMore InnerRegex
  | OneOrMore InnerRegex
  | Subgroup [InnerRegex]
  | Alternation InnerRegex InnerRegex
  | Space
  deriving (Show)

----------------------------------------------------------

-- | Parser for single inner regex
getInner :: CharParser (Either String) (Int, InnerRegex)
getInner =
  extensionInner simpleInner

-- | Get a extension of a pattern (optional, many, ...)
extensionInner ::
  CharParser (Either String) (Int, InnerRegex) ->
  CharParser (Either String) (Int, InnerRegex)
extensionInner prev = do
  (count, out) <- prev

  let mapSimple manipC constr str =
        extensionInner ((manipC count, constr out) <$ string str)
  let alter =
        Bf.bimap (min count) (Alternation out) <$ string "\\|" <*> getInner

  choice
    ( map
        extensionInner
        [ mapSimple (* 0) ZeroOrMore "\\*",
          mapSimple (* 2) OneOrMore "\\+",
          mapSimple (* 0) Optional "\\?",
          alter
        ]
    )
    <|> pure (count, out)

-- | Simple inner patterns
simpleInner :: CharParser (Either String) (Int, InnerRegex)
simpleInner =
  choice $
    ["\\@" =$> (1, Output), braces]
      ++ map
        (fmap (0,))
        [ "\\." =$> AnyNonSpace,
          "\\_" =$> Any,
          "\\^" =$> StartOfLine,
          "\\$" =$> EndOfLine,
          "\\\\" =$> Character '\\',
          Space <$ satisfy isSpace,
          Character <$> satisfy (/= '\\')
        ]
  where
    braces = do
      string "\\("
      r <- many getInner
      string "\\)"
      return $ Bf.bimap sum Subgroup $ unzip r

-- | Transform a string to pattern
getPattern :: String -> Either String [InnerRegex]
getPattern =
  parse (some getInner)
    >=> \case
      (0, _) -> Left "No output defined in regex"
      (1, o) -> Right o
      (_, _) -> Left "Too many outputs defined in regex"
      . Bf.first sum
      . unzip

----------------------------------------------------------

-- | Parser on Strings that outputs a list
type ListParser a = Parser [] Char a

-- | Yield the output of regex parser
yield :: ListParser String -> ListParser [String]
yield = ((: []) <$>)

-- | Regex-parser void (ignore the output)
loid :: ListParser o1 -> ListParser [o2]
loid = ([] <$)

----------------------------------------------------------

-- | Transform a InnerRegex to an actual parser
compile :: [InnerRegex] -> ListParser String
compile = foldl (liftA2 $ \xs x -> concat x ++ xs) ("" <$ epsilon) . map trans
  where
    trans :: InnerRegex -> Parser [] Char [String]
    trans =
      \case
        Character c -> loid $ char c
        Output -> yield $ some (satisfy (not . isSpace))
        AnyNonSpace -> loid $ satisfy (not . isSpace)
        Any -> loid succeed
        StartOfLine -> undefined -- TODO
        EndOfLine -> loid $ char '\n'
        Optional ir -> (trans ir <|> (succeed $> []))
        ZeroOrMore ir -> concat <$> many (trans ir)
        OneOrMore ir -> concat <$> some (trans ir)
        Subgroup irs -> yield $ compile irs
        Alternation ir1 ir2 -> trans ir1 <|> trans ir2
        Space -> loid $ satisfy isSpace

----------------------------------------------------------

-- | Run a parser and return the first match
regexFirst :: ListParser String -> String -> Maybe String
regexFirst parser str =
  case regexAll parser str of
    x:xs -> Just x
    [] -> Nothing

-- | Run a parser and return all matches
regexAll :: ListParser String -> String -> [String]
regexAll parser = map fst . runParser parser

----------------------------------------------------------

-- | Try to turn a Yaml Value to Pattern
fromConfig :: Yaml.Value -> PatternTry
fromConfig (Yaml.String str) =
  case getPattern (T.unpack str) of
    Left s -> PatternError s
    Right irs ->
      let compiled = compile irs
       in PatternSuccess $
            MkPatternMatcher (regexFirst compiled) (regexAll compiled)
fromConfig _ = IncorrectPattern

-- | The actual regex pattern
regexPattern :: Pattern
regexPattern = ("Regex", fromConfig)

----------------------------------------------------------

{- $
Missing output
>>> getPattern ""
Left ...

>>> getPattern "\\@\\*"
Left ...

>>> getPattern "\\@\\+"
Left ...

>>> getPattern "\\@\\|x"
Left ...

>>> getPattern "x\\|\\@"
Left ...

----------------------------------------------------------
Escaped characters
>>> getPattern "\\@\\\\"
Right [Output,Character '\\']

>>> getPattern"@\\@"
Right [Character '@',Output]

----------------------------------------------------------
Advanced patterns
>>> getPattern "\\^x\\@"
Right [StartOfLine,Character 'x',Output]

>>> getPattern "abc"
Left ...

>>> getPattern "\\^ \\(q\\)\\? i \\@"
Right [StartOfLine,Space,Optional (Subgroup [Character 'q']),Space,Character 'i',Space,Output]

>>> getPattern "\\@\\.\\*x"
Right [Output,ZeroOrMore AnyNonSpace,Character 'x']

>>> getPattern "\\(\\@a\\)\\|\\(\\@b\\)"
Right [Alternation (Subgroup [Output,Character 'a']) (Subgroup [Output,Character 'b'])]
-}
