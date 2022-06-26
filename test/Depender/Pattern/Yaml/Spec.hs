{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Depender.Pattern.Yaml.Spec (spec) where

import qualified Data.Aeson.Combinators.Encode as Enc
import Depender.Pattern (PatternMatcher (allMatch), PatternTry (..))
import Depender.Pattern.Yaml
import Test.Hspec
import Text.RawString.QQ

-- | Input test data
input :: String
input =
  [r|
simple_array:
  - val1
  - val2
  - val3

simple_object:
  key1: value1
  key2: value2
  key3: value3

nested_object1:
  object:
    - value

nested_object2:
  object:
    key: value

array_access:
  - array1
  - array2
  -
    - array3
    -
      - value1
      - value2
    - array4

simple_string: this_string

non_uniform:
  key1: 10
  key2: string
  key3: null
  key4: []
  key5:
    nested: object
|]

------------------------------------------------------------

-- | Input patterns
patt1, patt2, patt3, patt4, patt5, patt6, patt7 :: String
patt1 = ".simple_array"
patt2 = ".simple_object"
patt3 = ".nested_object1.object"
patt4 = ".nested_object2.object"
patt5 = ".array_access[2][1]"
patt6 = ".simple_string"
patt7 = ".non_uniform"

-- | Input iterators
itKeys, itValues, itArray, itString :: String
itKeys = "keys"
itValues = "values"
itArray = "array"
itString = "string"

------------------------------------------------------------

pattToVal :: String -> String -> PatternTry
pattToVal pattern iterate =
  parser $ Enc.run encoder (pattern, iterate)
  where
    (_, parser) = yamlPattern
    encoder =
      Enc.object
        [ Enc.field "pattern-type" Enc.string (const "yaml-parser"),
          Enc.field "path" Enc.string fst,
          Enc.field "iterate-over" Enc.string snd
        ]

runPatt :: String -> String -> Either String [String]
runPatt pattern iterate = case pattToVal pattern iterate of
  PatternSuccess pm -> Right $ allMatch pm input
  IncorrectPattern -> Left "IncorrectPattern"
  PatternError err -> Left $ "Error: " ++ err

isPatternSucc :: PatternTry -> Bool
isPatternSucc (PatternSuccess _) = True
isPatternSucc _ = False

instance Show PatternTry where
  show (PatternSuccess _) = "PatternSuccess"
  show IncorrectPattern = "IncorrectPattern"
  show (PatternError err) = "PatternError " ++ err

------------------------------------------------------------

spec :: SpecWith ()
spec = describe "Yaml Pattern" $ do
  it "Parses patterns" $ do
    pattToVal patt1 itKeys `shouldSatisfy` isPatternSucc
    pattToVal patt2 itKeys `shouldSatisfy` isPatternSucc
    pattToVal patt3 itKeys `shouldSatisfy` isPatternSucc
    pattToVal patt4 itKeys `shouldSatisfy` isPatternSucc
    pattToVal patt5 itKeys `shouldSatisfy` isPatternSucc
    pattToVal patt6 itKeys `shouldSatisfy` isPatternSucc
    pattToVal patt7 itKeys `shouldSatisfy` isPatternSucc
  it "Parses iterators" $ do
    let simple = ".x"
    pattToVal simple itKeys `shouldSatisfy` isPatternSucc
    pattToVal simple itValues `shouldSatisfy` isPatternSucc
    pattToVal simple itArray `shouldSatisfy` isPatternSucc
    pattToVal simple itString `shouldSatisfy` isPatternSucc
  it "Finds the correct value" $ do
    runPatt patt1 itArray `shouldBe` Right ["val1", "val2", "val3"]
    runPatt patt2 itKeys `shouldBe` Right ["key1", "key2", "key3"]
    runPatt patt2 itValues `shouldBe` Right ["value1", "value2", "value3"]
    runPatt patt3 itArray `shouldBe` Right ["value"]
    runPatt patt4 itKeys `shouldBe` Right ["key"]
    runPatt patt4 itValues `shouldBe` Right ["value"]
    runPatt patt5 itArray `shouldBe` Right ["value1", "value2"]
    runPatt patt6 itString `shouldBe` Right ["this_string"]
    runPatt patt7 itKeys `shouldBe` Right ["key1","key2","key3","key4","key5"]
