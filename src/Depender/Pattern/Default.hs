module Depender.Pattern.Default (defaultList) where

import Data.Yaml (Value)
import Depender.Pattern
import Depender.Pattern.Regex

defaultList :: [Pattern]
defaultList = [regexPattern]
