-- | Standard output writer package
module Depender.Writer.Stdout (stdoutWriter) where

import Data.List (intercalate)
import qualified Depender.DependencyGraph as G
import Depender.Writer (Writer (MkWriter))

transform :: G.Graph -> String
transform = concatMap goV . G.toList
  where
    goV (name, deps) = name ++ ": " ++ intercalate ", " deps ++ "\n"

-- | Simply output the structure to stdout, ignoring the output file
stdoutWriter :: Writer
stdoutWriter = MkWriter (const . putStrLn . transform)
