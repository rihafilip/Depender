{-# LANGUAGE QuasiQuotes #-}

module Depender.Configuration.Spec (spec) where

import Data.Either (isRight)
import Data.Yaml.Combinator
import Depender.Configuration
import qualified Depender.Pattern.Default as P
import qualified Depender.Writer.Default as W
import Test.Hspec
import Text.RawString.QQ

config :: String
config =
  [r|
elm-project:
    files:
        - "elm.json"
    patterns:
        - fixed-name: "project"
          pattern:
                pattern-type: yaml-parser
                path: ".dependencies.direct"
                iterate-over: keys

    type : stdout
    output: "dummy"

cpp:
    files:
        - "src/*.cpp"
        - "src/*.hpp"

    patterns:
        - pattern: \^#include <\@>
          file-name: with-extension

        - pattern: \^#include "\@"
          file-name: with-extension

    type: mermaid
    output: "doc/cpp-dep.dot"

haskell:
    files:
        - "backend/.hs"

    patterns:
        - pattern: \^ import \@
          name: \^ module \@ where

    type: Dot
    output: "doc/backend-dep.dot"
|]

spec = do
  describe "Configuration parsing" $ do
    it "Parses complex config" $ do
      runYamlDecoder (configurationDecoder P.defaultList W.defaultList) config
        `shouldSatisfy` isRight
