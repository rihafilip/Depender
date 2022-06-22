{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Depender.Configuration.Spec (spec) where

import qualified Data.ByteString.Char8 as B
import Data.Either (isRight)
import Data.Yaml (ParseException, decodeEither')
import Depender.Configuration
import Test.Hspec
import Text.RawString.QQ

config :: String
config =
  [r|
elm-project:
    files:
        - "elm.json"
    patterns:
        - type: module
          fixed-name: "project"
          pattern:
                type: json-parser
                json-path: ".dependencies.direct"
                iterate-over: keys

    type : UML
    output: "doc/project-dependecy.png"

cpp:
    files:
        - "src/*.cpp"
        - "src/*.hpp"

    patterns:
        - pattern: \^#include <\@>
          type: module
          file-name: with-extension

        - pattern: \^#include "\@"
          type: file
          file-name: with-extension

    type: dot
    output: "doc/cpp-dep.dot"

haskell:
    files:
        - "backend/.hs"

    patterns:
        - pattern: \^ import \@
          type: module
          name: \^ module \@ where

    type: dot
    output: "doc/backend-dep.dot"
|]

spec = do
  describe "Configuration parsing" $ do
    it "Parses complex config" $ do
      (decodeEither' (B.pack config) :: Either ParseException Configuration)
        `shouldSatisfy` isRight
