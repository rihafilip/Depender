{-# LANGUAGE TupleSections #-}

-- | The base IO driver of the program
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
  res <- runYamlDecoderFile (configurationDecoder ps ws) configFile
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
  let filteredFiles = runFileMatcher (fileMatcher conf) files
  forM_  filteredFiles (printInfoAbout "File found: ")
  filesAndCont <- mapM (\fp -> (fp,) <$> readFile fp) filteredFiles
  let graph = patternsToGraph (patterns conf) filesAndCont
  let out = outputFile conf
  printInfoAbout "Writing output to file " out
  let writer = writerType conf
  runWriter writer graph out
  printSuccess $ "Succesfuly written to " ++ out

-------------------------------------------------------------------------

-- | Recursively list all files in current working folder and it's subfolders
recursiveList :: FilePath -> IO [FilePath]
recursiveList fp = do
  ex <- doesFileExist fp
  if ex
    then pure [fp]
    else concat <$> (listDirectory fp >>= mapM (recursiveList . (fp </>)))

-------------------------------------------------------------------------

-- | Transform String to Chunk
toChunk :: String -> Chunk
toChunk = chunk . T.pack

-- | Pretty print an error
printError :: String -> IO ()
printError = putChunkLn . fore red . toChunk

-- | Pretty print an info
printInfo :: String -> IO ()
printInfo = putChunkLn . fore blue . toChunk

-- | Pretty print an info message about something
printInfoAbout :: String -> String -> IO ()
printInfoAbout info about =
  putChunk (fore blue $ toChunk info)
    >> putStrLn about

-- | Pretty print a success mesage
printSuccess :: String -> IO ()
printSuccess = putChunkLn . fore green . toChunk
