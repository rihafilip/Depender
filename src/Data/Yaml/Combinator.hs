module Data.Yaml.Combinator where

import Control.Monad ((>=>))
import qualified Data.Aeson.Combinators.Decode as Decode
import qualified Data.Bifunctor as Bf
import qualified Data.ByteString.Char8 as B
import qualified Data.Yaml as Yaml

runYamlDecoder :: Decode.Decoder a -> String -> Either String a
runYamlDecoder decoder =
  Bf.first show
    . Yaml.decodeEither'
    . B.pack
    >=> Decode.parseEither decoder

runYamlDecoderFile :: Decode.Decoder a -> FilePath -> IO (Either String a)
runYamlDecoderFile decoder fp =
  (Bf.first show >=> Decode.parseEither decoder)
    <$> Yaml.decodeFileEither fp
