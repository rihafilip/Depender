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
import Debug.Trace
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
  (count, out) <- (\p -> trace (show p) p) <$> prev

  let mapSimple constr str =
        extensionInner ((count, constr out) <$ string str)
  let alter =
        Bf.bimap (min count) (Alternation out) <$ string "\\|" <*> getInner

  choice
    ( map
        extensionInner
        [ mapSimple ZeroOrMore "\\*",
          mapSimple OneOrMore "\\+",
          mapSimple Optional "\\?",
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

-- | Run a Regex pattern and return a graph TODO
runRegex :: ListParser String -> [String] -> Graph
runRegex reg = undefined $
    map ((fst <$>). runParser reg)

----------------------------------------------------------

-- | Try to turn a Yaml Value to Pattern
fromConfig :: Yaml.Value -> PatternTry
fromConfig (Yaml.String str) =
  case getPattern (T.unpack str) of
    Left s -> PatternError s
    Right irs -> PatternSuccess (runRegex $ compile irs)
fromConfig _ = IncorrectPattern

-- | The actual regex pattern
regexPattern :: Pattern
regexPattern = MkPattern "Regex" fromConfig
