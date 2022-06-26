{-# LANGUAGE DeriveDataTypeable #-}

-- | Definitons of runnable CLI
module Depender.Main (defaultMain) where

import Data.Data
import qualified Depender.DependencyGraph as G
import qualified Depender.Driver as Driver
import Depender.Pattern (Pattern)
import Depender.Writer (Writer)
import System.Console.CmdArgs
import System.Directory
import Depender.Driver
import System.FilePath (makeRelative)

-- | Arguments type
data Depender = MkDepender
  { -- | Configs to run
    configurations :: [String],
    -- | Configuration file
    config :: FilePath,
    -- | Folder to run in
    folder :: FilePath
  }
  deriving (Show, Data, Typeable)

-- | Arguments
argConf wd =
  MkDepender
    { configurations = def &= args &= typ "CONFIGS",
      config = def &= typFile,
      folder = wd &= help "Specify different folder to run in" &= typDir
    }
    &= summary "Highly configurable dependency graph generator"

-- | Default main to run
defaultMain ::
  -- | List of all patterns
  [Pattern] ->
  -- | List of all writters
  [(String, Writer)] ->
  IO ()
defaultMain ps ws = do
  wd <- getCurrentDirectory
  putStrLn wd
  args <- cmdArgs (argConf wd)
  conf <- canonicalizePath $ config args
  let fold = makeRelative wd $ folder args
  configIsFile <- doesFileExist conf
  folderExists <- doesPathExist fold
  if not configIsFile
    then printError $ conf ++ " doesn't exist or is not a file"
    else if not folderExists
      then printError $ fold ++ " folder doesn't exist"
      else
        Driver.run ps ws (configurations args) conf fold
