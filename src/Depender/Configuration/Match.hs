{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Pure matching on Configuration
module Depender.Configuration.Match (patternsToGraph, configurationOfName) where

import Data.Function ((&))
import Depender.Configuration
import qualified Depender.DependencyGraph as G
import Depender.Pattern
import System.FilePath

-- | Get a name from name pattern, filepath and it's contents
getName :: PatternName -> FilePath -> String -> Maybe String
getName pName fp content =
  case pName of
    FixedName s -> Just s
    FileNameWithExtension -> Just $ takeFileName fp
    FileNameNoExtension -> Just $ takeBaseName fp
    NameFromPattern pm -> firstMatch pm content

------------------------------------------------------------------

-- | Many patterns, files and it's contents to graph
patternsToGraph ::
  -- | Patterns to use
  [NamedPattern] ->
  -- | List of (file path, content)
  [(FilePath, String)] ->
  -- | Output graph or error
  Either String G.Graph
patternsToGraph patts files = foldr (\p g -> patternToGraph p g files) (Right G.empty) patts

patternToGraph :: NamedPattern -> Either String G.Graph -> [(FilePath, String)] -> Either String G.Graph
patternToGraph patt = foldr ((=<<) . patternGo patt)

patternGo :: NamedPattern -> (FilePath, String) -> G.Graph -> Either String G.Graph
patternGo MkNamedPattern {pName, actualPattern} (fp, content) graph =
  maybe
    (Left $ "No name found for file '" ++ fp ++ "'")
    (Right . add)
    $ getName pName fp content
  where
    matches = allMatch actualPattern content
    add name = G.addDependecies graph name matches

------------------------------------------------------------------

-- | Extract a configuration of given name
configurationOfName :: Configuration -> String -> Either String SingleConfiguration
configurationOfName conf name =
  filter (\MkSingleConf {cName} -> name == cName) conf
    & \case
      [] -> Left $ "No configuration of name '" ++ name ++ "'"
      [c] -> Right c
      _ -> Left $ "Two or more configurations of name '" ++ name ++ "'"
