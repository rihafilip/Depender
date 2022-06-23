{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Depender.Driver (run, printError, printSuccess, printInfo) where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (forM_, (>=>))
import Data.Foldable (foldrM)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Text as T
import Data.Yaml.Combinator
import Depender.Configuration
import qualified Depender.DependencyGraph as G
import Depender.File.Glob (FileMatcher (runFileMatcher))
import Depender.Pattern
import Depender.Writer (Writer, runWriter)
import Rainbow
import System.Directory
import System.FilePath (takeBaseName, takeFileName)

--  TODO maybe remove pattern type?

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
run ps ws runThis configFile workingDir =
  let decoder = configurationDecoder ps ws
   in runYamlDecoderFile decoder configFile
        >>= either
          (printError . ("Error while parsing configuration file:" ++))
          (\c -> runConf c runThis workingDir)

-------------------------------------------------------------------------

-- | Run a configuration
runConf :: Configuration -> [String] -> FilePath -> IO ()
runConf conf runThis workingDir =
  recursiveList workingDir >>= \files ->
    forM_ runThis $ \n ->
      filter (\MkSingleConf {cName} -> n == cName) conf
        & \case
          [] -> printError $ "No configuration of name '" ++ n ++ "'"
          [c] -> runSingleConf c files
          _ -> printError $ "Two or more configurations of name '" ++ n ++ "'"
  where
    -- Recursively list all files in current working folder and it's subfolders
    recursiveList :: FilePath -> IO [FilePath]
    recursiveList fp = do
      ex <- doesFileExist fp
      if ex
        then pure [fp]
        else concat <$> (listDirectory fp >>= mapM recursiveList)

-- | Helper monad manipulation
(|>>) :: Either a b -> (b -> IO (Either a b)) -> IO (Either a b)
x |>> f = pure x >>= either (pure . Left) f

-- | Run a single configuration
runSingleConf :: SingleConfiguration -> [FilePath] -> IO ()
runSingleConf MkSingleConf {cName, files, patterns, writerType, outputFile} workingFiles = do
  printInfo $ "Processing configuration " ++ cName
  let filteredFiles = runFileMatcher files workingFiles
  foldrM (\file -> (|>> runPatterns patterns file)) (Right G.empty) filteredFiles
    >>= \case
      Left err -> printError $ "Error proccessing configuration: " ++ err
      Right gr -> do
        printInfo ("Writing output to file" ++ outputFile)
        runWriter writerType gr outputFile
        printInfo ("Succesfuly written to " ++ outputFile)

-- | Run a list of patterns on file
runPatterns :: [PatternWithMetadata] -> FilePath -> G.Graph -> IO (Either String G.Graph)
runPatterns patts fp graph = foldrM go (Right graph) patts
  where
    go p = (|>> runSinglePattern p fp)

-- | Run a single pattern on one file
runSinglePattern :: PatternWithMetadata -> FilePath -> G.Graph -> IO (Either String G.Graph)
runSinglePattern MkPatternWMD {patternType, pName, actuallPattern} fp graph =
  readFile fp <&> \fileContent ->
    let name = case pName of
          FixedName s -> Just s
          FileNameWithExtension -> Just $ takeFileName fp
          FileNameNoExtension -> Just $ takeBaseName fp
          NameFromPattern pm -> firstMatch pm fileContent
        matched = allMatch actuallPattern fileContent
        fileErr = "Current module name not found in file " ++ fp
     in flip (G.addDependecies graph) matched
          <$> maybe (Left fileErr) Right name

-------------------------------------------------------------------------

toChunk :: String -> Chunk
toChunk = chunk . T.pack

printError :: String -> IO ()
printError = putChunkLn . fore red . toChunk

printInfo :: String -> IO ()
printInfo = putChunkLn . fore blue . toChunk

printSuccess :: String -> IO ()
printSuccess = putChunkLn . fore green . toChunk
