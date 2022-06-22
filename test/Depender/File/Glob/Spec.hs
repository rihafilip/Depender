module Depender.File.Glob.Spec (spec) where

import Depender.File.Glob
import System.FilePath
import Test.Hspec

infix 1 ~=

patt ~= dir =
  maybe
    []
    (\f -> runFileMatcher f [dir])
    (globFilterFPs [patt])
    `shouldBe` [dir]

infix 1 ~!=
patt ~!= dir =
  maybe
    []
    (\f -> runFileMatcher f [dir])
    (globFilterFPs [patt])
    `shouldBe` []

spec = describe "Glob Matching" $ do
  it "Plain text" $ do
    "x" ~= "x"
    "x/y" ~= "x" </> "y"
    "x/" ~= "x"
    "/x" ~!= "x"
  it "Basic wildcard expansion" $ do
    "*" ~= "x"
    "**" ~= "abc"
    "**" ~= "a/b/c"
    "?" ~= "x"
  it "More advanced patterns" $ do
    "src/***.hs" ~= "src/test.hs"
    "src/**/*.hs" ~= "src/test.hs"
    "src/**/*.hs/" ~= "src/test.hs"
    "**x**" ~= "abc/de/f/gx"
    "**x**" ~= "abc/dxe/f/g"
