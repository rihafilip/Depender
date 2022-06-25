-- | File path with globs matcher
module Depender.File.Glob (FileMatcher, runFileMatcher, globFilterFPs) where

import Control.Applicative
import Data.Maybe (mapMaybe)
import System.FilePath
import Text.Parser

-- | File match format
data Format
  = DirectorySep
  | Star
  | DoubleStar
  | SingleCharacter
  | Character Char
  deriving (Show, Eq)

-- | Try to turn string into Format
formatParser :: CharParser Maybe [Format]
formatParser =
  some $
    choice
      [ "/" =$> DirectorySep,
        "**" =$> DoubleStar,
        "*" =$> Star,
        "?" =$> SingleCharacter,
        Character <$> succeed
      ]

-- | match one format on one filepath
globFP :: [Format] -> FilePath -> Bool
globFP [] [] = True
globFP [] _ = False
globFP (f : fs) []
  | f `elem` [Star, DoubleStar, DirectorySep] = globFP fs []
  | otherwise = False
globFP (f : fs) (x : xs) =
  case f of
    DirectorySep | sep -> globFP fs xs
    DoubleStar -> globFP (f : fs) xs || globFP fs (x : xs)
    Star | sep -> globFP fs (x : xs)
    Star -> globFP (f : fs) xs || globFP fs (x : xs)
    SingleCharacter | not sep -> globFP fs xs
    Character ch -> ch == x && globFP fs xs
    _ -> False
  where
    sep = isPathSeparator x

-- | Optimize and fix patterns "***", "**/"
canonizeGlob :: [Format] -> [Format]
canonizeGlob [] = []
canonizeGlob (DoubleStar : DirectorySep : fs) = canonizeGlob (DoubleStar : fs)
canonizeGlob (DoubleStar : DoubleStar : fs) = canonizeGlob (DoubleStar : fs)
canonizeGlob (Star : DoubleStar : fs) = canonizeGlob (DoubleStar : fs)
canonizeGlob (DoubleStar : Star : fs) = canonizeGlob (DoubleStar : fs)
canonizeGlob (f : fs) = f : canonizeGlob fs

-- | The actual file matcher type
newtype FileMatcher = MkFileMatcher { runFileMatcher :: [FilePath] -> [FilePath] }

instance Show FileMatcher where
  show _ = "<fileMatcher>"

-- | Filter a list of filepaths thru a list of file patterns,
-- returning Nothing if the patterns are incorrect
globFilterFPs :: [String] -> Maybe FileMatcher
globFilterFPs strs
  | length formats /= length strs = Nothing
  | otherwise = Just . MkFileMatcher $ filter matchesAny . map normalise
  where
    formats = map canonizeGlob $ mapMaybe (parse formatParser) strs
    matchesAny d = any (`globFP` d) formats
