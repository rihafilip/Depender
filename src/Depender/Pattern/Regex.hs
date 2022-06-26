{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Regex pattern matcher
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
import Depender.Pattern.Regex.Inner

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


-- | Run regex and return the first match
runInnerRegexFirst :: [InnerRegex] -> String -> Maybe String
runInnerRegexFirst irs str = case runInnerRegex irs str of
  [] -> Nothing
  x : _ -> Just x

----------------------------------------------------------

-- | Try to turn a Yaml Value to Pattern
fromConfig :: Yaml.Value -> PatternTry
fromConfig (Yaml.String str) =
  case getPattern (T.unpack str) of
    Left s -> PatternError s
    Right irs ->
      PatternSuccess $
        MkPatternMatcher (runInnerRegexFirst irs) (runInnerRegex irs)
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
>>> getPattern "abc"
Left ...

>>> getPattern " \\(q\\)\\? i \\@"
Right [Space,Optional (Subgroup [Character 'q']),Space,Character 'i',Space,Output]

>>> getPattern "\\@\\.\\*x"
Right [Output,ZeroOrMore AnyNonSpace,Character 'x']

>>> getPattern "\\(\\@a\\)\\|\\(\\@b\\)"
Right [Alternation (Subgroup [Output,Character 'a']) (Subgroup [Output,Character 'b'])]
-}
