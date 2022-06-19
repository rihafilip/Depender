module Depender.Pattern where

import qualified Data.Yaml as Yaml

-- | Data of one pattern - it's name and function to extraxt PatternMatcher
type Pattern = (String, Yaml.Value -> PatternTry)

data PatternMatcher = MkPatternMatcher
  { firstMatch :: String -> Maybe String,
    allMatch :: String -> [String]
  }

-- | Result of trying to extract a pattern
data PatternTry
  = PatternSuccess PatternMatcher -- Correct pattern -> matching funciton
  | IncorrectPattern -- Not a pattern of given type
  | PatternError String -- Pattern of given type but with incorrect structure

-- | Try to get a successful pattern from list of them
getPattern :: Yaml.Value -> [Pattern] -> Either String PatternMatcher
getPattern v =
  toEither
    . foldl go IncorrectPattern -- get first correct pattern
    . map (\(_, fromConfig) -> fromConfig v)
  where
    go IncorrectPattern r = r
    go l _ = l
    toEither (PatternSuccess s) = Right s
    toEither IncorrectPattern = Left "No matching pattern found"
    toEither (PatternError e) = Left e
