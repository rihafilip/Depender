module Depender.Configuration where

import Depender.Pattern

-- | Dummy Writer type TODO
data WriterType

-- | The complete configuration
type Configuration = [SingleConfiguration]

-- | One single configuration
data SingleConfiguration = MkSC
  { cName :: String,
    files :: [FilePath],
    patterns :: [PatternWithMetadata],
    writterType :: WriterType,
    outputFile :: FilePath
  }

-- | Pattern with it's name and type
data PatternWithMetadata = MkPatternWMD
  { patternType :: PatternType
  , pName :: PatternName
  , actuallPattern :: PatternFunction
  }

-- | If the patterns works on file or module structure
data PatternType = Module | File

-- | Different types of pattern naming
data PatternName
  = FixedName String
  | FileNameWithExtension
  | FileNameNoExtension
  | NameFromPattern PatternFunction
