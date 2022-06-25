module Main where

import qualified Depender.Pattern.Default as P
import qualified Depender.Writer.Default as W
import Depender.Main

main :: IO ()
main = defaultMain P.defaultList W.defaultList
