module Depender.Pattern where

import Depender.DependencyGraph

import qualified Data.Yaml as Yaml

-- | Data of one pattern
data Pattern = MkPattern
  { patternName :: String
  , fromConfig :: Yaml.Value -> PatternTry
  }

-- | Get a list of files with contents and return
type PatternFunction = [String] -> Graph

-- | Result of trying to extract a pattern
data PatternTry
  = PatternSuccess PatternFunction -- * Correct pattern -> matching funciton
  | IncorrectPattern -- * Not a pattern of given type
  | PatternError String -- * Pattern of given type but with incorrect structure

-- | Try to get a successful pattern from list of them
getPattern :: Yaml.Value -> [Pattern] -> Either String ([String] -> Graph)
getPattern v = toEither . foldl go IncorrectPattern . map (\p -> fromConfig p v)
  where
    go IncorrectPattern r = r
    go l _ = l
    toEither (PatternSuccess s) = Right s
    toEither IncorrectPattern = Left "No matching pattern found"
    toEither (PatternError e) = Left e
