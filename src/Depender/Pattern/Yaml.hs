{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | YAML and JSON pattern matcher
module Depender.Pattern.Yaml (yamlPattern) where

import Control.Applicative
import Control.Monad ((>=>))
import Data.Aeson (Value, withArray, withObject, withText, (.:))
import qualified Data.Aeson.Combinators.Decode as Decode
import Data.Aeson.Key (fromString, toString)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (JSONPathElement (..))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.Either (fromRight)
import Data.Foldable (Foldable (toList))
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Yaml.Combinator (runYamlDecoder)
import qualified Depender.DependencyGraph as G
import Depender.Pattern (Pattern, PatternMatcher (MkPatternMatcher), PatternTry (..))
import Text.Parser

-- $
-- >>> parse jsonPathParser ".x"
-- Just [Key "x"]
--
-- >>> parse jsonPathParser "[1]"
-- Just [Index 1]
--
-- >>> parse jsonPathParser ".x.y"
-- Just [Key "x",Key "y"]
--
-- >>> parse jsonPathParser "[1][2]"
-- Just [Index 1,Index 2]
--
-- >>> parse jsonPathParser ".x[1]"
-- Just [Key "x",Index 1]
--
-- >>> parse jsonPathParser "[1].x"
-- Just [Index 1,Key "x"]
--
-- >>> parse jsonPathParser ".'ab. \" []'"
-- Just [Key "ab. \" []"]
--
-- >>> parse jsonPathParser ".\" ab . ' []\""
-- Just [Key " ab . ' []"]

-- | Parser for json
jsonPathParser :: Parser Maybe Char [JSONPathElement]
jsonPathParser =
  some $
    choice
      [ k <$= ".\"" <*> quoted '"' <* char '"',
        k <$= ".'" <*> quoted '\'' <* string "'",
        k <$= "." <*> word,
        Index <$= "[" <*> integer <* char ']'
      ]
  where
    k = Key . fromString
    word = some (satisfy (`notElem` ['.', '[']))
    quoted ch = some (satisfy (/= ch))
    integer = read <$> some (satisfy isDigit)

-----------------------------------------------------------

-- | Match the type to iterate over
matchIterate :: MonadFail m => String -> m (Decode.Decoder [String])
matchIterate = \case
  "keys" -> pure iterateKeys
  "values" -> pure iterateValues
  "array" -> pure iterateArray
  "string" -> pure iterateString
  _ -> fail "Unknown type of 'iterate-over'"
  where
    iterateKeys =
      map (toString . fst) . KM.toList
        <$> Decode.keyMap (Decode.Decoder pure)

    iterateValues =
      map snd . KM.toList
        <$> Decode.keyMap Decode.string

    iterateArray = Decode.list Decode.string
    iterateString = (: []) <$> Decode.string

-----------------------------------------------

-- | Parse the whole JSON/YAML configuration
parseConfiguration :: Value -> Aeson.Parser PatternTry
parseConfiguration (Aeson.Object o) =
  -- Verify this is the correct pattern type
  o .: "pattern-type"
    >>= \name ->
      if (name :: String) /= "yaml-parser"
        then pure IncorrectPattern
        else getRest
  where
    getRest = do
      -- get what to iterate over
      iter <- o .: "iterate-over" >>= matchIterate
      -- parse the json path
      jsonPath <-
        o .: "path"
          >>= maybe
            (fail "Not a correct path")
            pure
            . parse jsonPathParser
      return $
        PatternSuccess $
          MkPatternMatcher
            (matchOne jsonPath iter)
            (matchAll jsonPath iter)
parseConfiguration _ = pure IncorrectPattern

-- | Get all matches
matchAll :: Aeson.JSONPath -> Decode.Decoder [String] -> String -> [String]
matchAll jsonPath iterator =
  fromRight []
    . runYamlDecoder (Decode.path jsonPath iterator)

-- | Get the first match
matchOne :: Aeson.JSONPath -> Decode.Decoder [String] -> String -> Maybe String
matchOne jsonPath iterator str =
  matchAll jsonPath iterator str & \case
    [] -> Nothing
    (x : _) -> Just x

-- | Run the configuration parser
configurationParser :: Value -> PatternTry
configurationParser =
  either PatternError id
    . Decode.parseEither (Decode.Decoder parseConfiguration)

-- | The actual pattern
yamlPattern :: Pattern
yamlPattern = ("yaml", configurationParser)
