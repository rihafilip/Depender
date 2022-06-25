{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Depender.Driver (run, printError, printSuccess, printInfo) where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (forM_, (>=>))
import Data.Foldable (foldrM)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Text as T
import Data.Yaml.Combinator
import Depender.Configuration
import Depender.Configuration.Match
import qualified Depender.DependencyGraph as G
import Depender.File.Glob (FileMatcher (runFileMatcher))
import Depender.Pattern
import Depender.Writer (Writer, runWriter)
import Rainbow
import System.Directory
import System.FilePath (takeBaseName, takeFileName, (</>))
import System.IO (readFile')

-- | Run a configuration in a file
run ::
  -- | List of all patterns
  [Pattern] ->
  -- | List of all writters
  [(String, Writer)] ->
  -- | List of configurations to run
  [String] ->
  -- | Filepath to configuration
  FilePath ->
  -- | Working directory
  FilePath ->
  IO ()
run ps ws runThis configFile workingDir = do
  printInfoAbout "Trying to parse a config file: " configFile
  cont <- readFile' configFile
  let res = runYamlDecoder (configurationDecoder ps ws) cont
  either
    (printError . ("Error while parsing configuration file:" ++))
    (\c -> runConf c runThis workingDir)
    res

-- | Run a configuration
runConf :: Configuration -> [String] -> FilePath -> IO ()
runConf conf runThis workingDir = do
  files <- recursiveList workingDir
  forM_ runThis $
    either
      printError
      (runSingleConf files)
      . configurationOfName conf

-- | Run a single configuration
runSingleConf :: [FilePath] -> SingleConfiguration -> IO ()
runSingleConf files conf = do
  printInfo $ "Processing configuration '" ++ cName conf ++ "'"
  filesAndCont <- mapM (\fp -> (fp,) <$> readFile fp) files
  either
    printError
    (finishAndWrite conf)
    (patternsToGraph (patterns conf) filesAndCont)

-- | Finish the
finishAndWrite :: SingleConfiguration -> G.Graph -> IO ()
finishAndWrite MkSingleConf {writerType, outputFile} graph =
  doesFileExist outputFile >>= \exists ->
    if not exists
      then printError $ "Output file '" ++ outputFile ++ "' does not exist"
      else do
        printInfoAbout "Writing output to file" outputFile
        runWriter writerType graph outputFile
        printInfo ("Succesfuly written to " ++ outputFile)

-------------------------------------------------------------------------

-- | Recursively list all files in current working folder and it's subfolders
recursiveList :: FilePath -> IO [FilePath]
recursiveList fp = do
  printInfoAbout "Recursively found: " fp
  ex <- doesFileExist fp
  if ex
    then pure [fp]
    else concat <$> (listDirectory fp >>= mapM (recursiveList . (fp </>)))

-------------------------------------------------------------------------

toChunk :: String -> Chunk
toChunk = chunk . T.pack

printError :: String -> IO ()
printError = putChunkLn . fore red . toChunk

printInfo :: String -> IO ()
printInfo = putChunkLn . fore blue . toChunk

printInfoAbout :: String -> String -> IO ()
printInfoAbout info about =
  putChunk (fore blue $ toChunk info)
    >> putStrLn about

printSuccess :: String -> IO ()
printSuccess = putChunkLn . fore green . toChunk
