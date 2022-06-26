{-# LANGUAGE LambdaCase #-}

-- | Inner representation of regex
module Depender.Pattern.Regex.Inner (InnerRegex (..), runInnerRegex) where

import Control.Applicative (Alternative (empty), (<|>))
import qualified Data.Bifunctor as Bf
import Data.Char (isSpace)

-- | Different types of regular expressions that are possible
data InnerRegex
  = Character Char
  | Output
  | AnyNonSpace
  | Any
  | EndOfLine
  | Optional InnerRegex
  | ZeroOrMore InnerRegex
  | OneOrMore InnerRegex
  | Subgroup [InnerRegex]
  | Alternation InnerRegex InnerRegex
  | Space
  deriving (Show)

-- | Run regex and return all matches
runInnerRegex :: [InnerRegex] -> String -> [String]
runInnerRegex pat =
  \case
    [] -> []
    (x : xs) ->
      case regexDriver pat (x : xs) of
        Nothing -> go xs
        Just (a, b) -> a : go b
  where
    go = runInnerRegex pat
    regexDriver :: [InnerRegex] -> String -> Maybe (String, String)
    regexDriver _ [] = Nothing
    regexDriver p xss@(_ : xs) =
      stack p xss
        <|> regexDriver p xs

(<:>) :: Char -> Maybe (String, o) -> Maybe (String, o)
(<:>) x = (Bf.first (x:) <$>)

stack :: [InnerRegex] -> String -> Maybe (String, String)
stack [] xs = Just ([], xs)
-- stack _ [] = empty
stack (firstPattern : ps) inputString =
  case (firstPattern, inputString) of
    (Character ch, x : xs)
      | ch == x -> stack ps xs
    (Character _, _) ->
      empty
    (Output, x : xs)
      | isSpace x -> stack ps (x:xs)
      | otherwise -> x <:> stack (Output : ps) xs
        <|> stack ps (x:xs)
    (Output, [])
      -> stack ps []
    (AnyNonSpace, x : xs)
      | not $ isSpace x -> stack ps xs
    (AnyNonSpace, _) ->
      empty
    (Any, x : xs) ->
      stack ps xs
    (Any, []) ->
      empty
    (EndOfLine, '\n' : xs) ->
      stack ps xs
    (EndOfLine, _) ->
      empty
    (Optional p, xss) ->
      stack (p : ps) xss
        <|> stack ps xss
    (z@(ZeroOrMore p), xss) ->
      stack (p : z : ps) xss
        <|> stack ps xss
    (OneOrMore p, xss) ->
      stack (p : ZeroOrMore p : ps) xss
    (Subgroup p, xss) ->
      stack (p ++ ps) xss
    (Alternation p1 p2, xss) ->
      stack (p1 : ps) xss
        <|> stack (p2 : ps) xss
    (Space, xss) ->
      case span isSpace xss of
        ([], _) -> empty
        (_, rest) -> stack ps rest

----------------------------------------------

-- $
-- >>> let (~=) p = stack [p]
-- >>> Character 'x' ~= "xx"
-- Just ("","x")
-- >>> Character 'x' ~= "y"
-- Nothing
--
-- >>> Output ~= "abc d"
-- Just ("abc"," d")
--
-- >>> AnyNonSpace ~= "xy"
-- Just ("","y")
-- >>> AnyNonSpace ~= " x"
-- Nothing
--
-- >>> Any ~= "xy"
-- Just ("","y")
-- >>> Any ~= " x"
-- Just ("","x")
--
-- >>> EndOfLine ~= "\nx"
-- Just ("","x")
-- >>> EndOfLine ~= "xx"
-- Nothing
--
-- >>> Optional (Character 'x') ~= "xx"
-- Just ("","x")
-- >>> Optional (Character 'x') ~= "yy"
-- Just ("","yy")
--
-- >>> ZeroOrMore (Character 'x') ~= "xxy"
-- Just ("","y")
-- >>> ZeroOrMore (Character 'x') ~= "yy"
-- Just ("","yy")
--
-- >>> OneOrMore (Character 'x') ~= "xxy"
-- Just ("","y")
-- >>> OneOrMore (Character 'x') ~= "yy"
-- Nothing
--
-- >>> let sub = Subgroup [Character 'x', Character 'y']
-- >>> sub ~= "xyz"
-- Just ("","z")
-- >>> sub ~= "xxx"
-- Nothing
--
-- >>> let al = Alternation (Character 'a') (Character 'b')
-- >>> al ~= "ax"
-- Just ("","x")
-- >>> al ~= "bx"
-- Just ("","x")
-- >>> al ~= "xx"
-- Nothing
--
-- >>> Space ~= "   x"
-- Just ("","x")
-- >>> Space ~= "x"
-- Nothing
