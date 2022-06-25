{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

module Depender.Main (defaultMain) where

import Data.Data
import qualified Depender.DependencyGraph as G
import qualified Depender.Driver as Driver
import Depender.Pattern (Pattern)
import Depender.Writer (Writer)
import System.Console.CmdArgs
import System.Directory
import Depender.Driver

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
      folder = def &= help "Specify different folder to run in" &= opt wd &= typDir
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
  args <- cmdArgs (argConf wd)
  print args
  conf <- canonicalizePath $ config args
  fold <- canonicalizePath $ folder args
  configIsFile <- doesFileExist conf
  folderExists <- doesPathExist fold
  if not configIsFile
    then printError $ conf ++ " doesn't exist or is not a file"
    else if not folderExists
      then printError $ fold ++ " doesn't exist"
      else
        Driver.run ps ws (configurations args) conf fold
