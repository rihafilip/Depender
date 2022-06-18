module Depender.Configuration where
import Depender.Pattern (PatternFunction)

data WriterType

type Configuration = [SingleConfiguration]

data SingleConfiguration = MkSC
  { cName :: String,
    patterns :: [Pattern],
    writterType :: WriterType,
    outputFile :: FilePath
  }

data PatternType = Module | File

data PatternName
  = FixedName String
  | FileName
  | NameFromPattern PatternFunction

data Pattern = MkPattern
  { patternType :: PatternType
  , pName :: PatternName
  , actuallPattern :: PatternFunction
  }
