module Depender.Writer.Stdout(stdoutWriter) where
import Depender.Writer (Writer(MkWriter))
import qualified Depender.DependencyGraph as G
import Data.List (intercalate)

transform :: G.Graph -> String
transform = concatMap goV . G.toList
  where goV (name, deps) = name ++ ": " ++ intercalate ", " deps ++ "\n"

-- | Simply output the structure to stdout
stdoutWriter :: Writer
stdoutWriter = MkWriter ( const . putStrLn . transform )
