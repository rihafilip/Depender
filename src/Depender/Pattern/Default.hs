module Depender.Pattern.Default (defaultList) where

import Data.Yaml (Value)
import Depender.Pattern (Pattern)
import Depender.Pattern.Regex (regexPattern)
import Depender.Pattern.Yaml (yamlPattern)

-- | Default list of patterns
defaultList :: [Pattern]
defaultList = [regexPattern, yamlPattern]
