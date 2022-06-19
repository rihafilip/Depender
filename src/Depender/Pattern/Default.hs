module Depender.Pattern.Default (defaultList) where

import Data.Yaml (Value)
import Depender.Pattern
import Depender.Pattern.Regex

-- | Default list of patterns
defaultList :: [Pattern]
defaultList = [regexPattern]
