-- |
--  A YAML helpers, allowing to use 'Data.Aeson.Combinators.Decode.Decoder' with
--  YAML files and strings
module Data.Yaml.Combinator where

import Control.Monad ((>=>))
import qualified Data.Aeson.Combinators.Decode as Decode
import qualified Data.Bifunctor as Bf
import qualified Data.ByteString.Char8 as B
import qualified Data.Yaml as Yaml

-- | Decode a YAML string using a decoder
runYamlDecoder :: Decode.Decoder a -> String -> Either String a
runYamlDecoder decoder =
  Bf.first show
    . Yaml.decodeEither'
    . B.pack
    >=> Decode.parseEither decoder

-- | Decode a YAML file using a decoder
runYamlDecoderFile :: Decode.Decoder a -> FilePath -> IO (Either String a)
runYamlDecoderFile decoder fp =
  (Bf.first show >=> Decode.parseEither decoder)
    <$> Yaml.decodeFileEither fp
