{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The whole configuration types

module Depender.Configuration
  ( -- * Configuration types
    Configuration (..),
    SingleConfiguration (..),
    NamedPattern (..),
    PatternName (..),

    -- * Configuration decoder
    configurationDecoder,
  )
where

import Control.Applicative (Alternative ((<|>)), empty)
import qualified Data.Aeson.Combinators.Decode as Comb
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Bifunctor as Bf
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Yaml ((.:), (.:?))
import qualified Data.Yaml as Yaml
import Depender.File.Glob
import Depender.Pattern (Pattern, PatternMatcher, getPattern)
import qualified Depender.Pattern.Default as Pattern (defaultList)
import Depender.Writer as Writer (Writer, getWriter)
import qualified Depender.Writer.Default as Writer (defaultList)

-- | The complete configuration
type Configuration = [SingleConfiguration]

-- | One single configuration
data SingleConfiguration = MkSingleConf
  { cName :: String,
    fileMatcher :: FileMatcher,
    patterns :: [NamedPattern],
    writerType :: Writer,
    outputFile :: FilePath
  }
  deriving (Show)

-- | Pattern with it's name and type
data NamedPattern = MkNamedPattern
  { pName :: PatternName,
    actualPattern :: PatternMatcher
  }
  deriving (Show)

-- | Different types of pattern naming
data PatternName
  = FixedName String
  | FileNameWithExtension
  | FileNameNoExtension
  | NameFromPattern PatternMatcher
  deriving (Show)

--------------------------------------------

-- | Utility function to turn Maybe to Parser
mbyToPars :: String -> Maybe a -> Yaml.Parser a
mbyToPars str = maybe (fail str) pure

-- | Utility function to turn Either to Parser
eiToPars :: Either String a -> Yaml.Parser a
eiToPars = either fail pure

--------------------------------------------

-- | Parse a Pattern with metadata
parsePattern :: [Pattern] -> Yaml.Value -> Yaml.Parser NamedPattern
parsePattern ps =
  Yaml.withObject "Pattern" $ \o -> do
    fileName <-
      o .:? "file-name"
        >>= mapM (Yaml.withText "File name type" matchFileName)

    fixedName <- o .:? "fixed-name" <&> fmap FixedName

    namePattern <-
      o .:? "name"
        >>= mapM (fmap NameFromPattern . eiToPars . flip getPattern ps)

    -- Get exactly one name specifier
    pName <-
      case catMaybes [fileName, fixedName, namePattern] of
        [x] -> pure x
        [] -> fail "Missing a file name specifier"
        _ -> fail "Too many name specifiers"

    actualPattern <- o .: "pattern" >>= eiToPars . flip getPattern ps

    return MkNamedPattern {pName, actualPattern}
  where
    matchType _ = fail "Not a valid pattern type"

    matchFileName "with-extension" = pure FileNameWithExtension
    matchFileName "without-extension" = pure FileNameNoExtension
    matchFileName _ = fail "Not a valid file name type"

-- | Parse a single configuration
parseSingleConfiguration ::
  [Pattern] ->
  [(String, Writer)] ->
  String ->
  Yaml.Value ->
  Yaml.Parser SingleConfiguration
parseSingleConfiguration ps ws name =
  Yaml.withObject ("Configuration " ++ name) $ \o -> do
    (outputFile :: String) <- o .: "output"

    fileMatcher <-
      o .: "files"
        >>= mbyToPars "Error in file format" . globFilterFPs

    patterns <-
      o .: "patterns"
        >>= Yaml.withArray "Patterns" (mapM (parsePattern ps) . toList)

    writerType <-
      o .: "type"
        >>= Yaml.withText
          "Output type"
          (mbyToPars "Non-existent output type" . getWriter ws . T.unpack)

    return MkSingleConf {cName = name, fileMatcher, patterns, writerType, outputFile}

-- | Parse the whole configuration file
parseConfiguration ::
  [Pattern] ->
  [(String, Writer)] ->
  Yaml.Value ->
  Yaml.Parser Configuration
parseConfiguration ps ws =
  Yaml.withObject "Configurations" $
    \o -> mapM go (KeyMap.toList o)
  where
    go =
      uncurry (parseSingleConfiguration ps ws)
        . Bf.first Key.toString

-- | Get a Aeson Combinator decoder for list of patterns and writers
configurationDecoder ::
  [Pattern] ->
  [(String, Writer)] ->
  Comb.Decoder Configuration
configurationDecoder ps ws = Comb.Decoder $ parseConfiguration ps ws
