module Depender.Configuration where

import Depender.Pattern

data WriterType

type Configuration = [SingleConfiguration]

data SingleConfiguration = MkSC
  { cName :: String,
    files :: [FilePath],
    patterns :: [PatternWithMetadata],
    writterType :: WriterType,
    outputFile :: FilePath
  }

data PatternWithMetadata = MkPatternWMD
  { patternType :: PatternType
  , pName :: PatternName
  , actuallPattern :: PatternFunction
  }

data PatternType = Module | File

data PatternName
  = FixedName String
  | FileNameWithExtension
  | FileNameNoExtension
  | NameFromPattern PatternFunction
