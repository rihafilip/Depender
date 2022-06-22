{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

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
  | StartOfLine
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
      matchRegexList p xss
        <|> regexDriver p xs

-- | Lift a rest of output to regex match output
yield :: String -> Maybe (String, String)
yield = Just . ([],)

-- | Match a list of regex pattern, returning a Just (output, rest of input)
-- on success or Nothing on failure
matchRegexList :: [InnerRegex] -> String -> Maybe (String, String)
matchRegexList [] xs = yield xs
matchRegexList _ [] = empty
matchRegexList (p : ps) xss = p ~= xss +> matchRegexList ps

infixl 6 ~=

-- | Try to match a single pattern to givcen string
(~=) :: InnerRegex -> String -> Maybe (String, String)
-- Match character
Character ch ~= (x : xs)
  | ch == x = yield xs
Character _ ~= _ =
  empty
-- Match \.\+ to output
Output ~= xss =
  matchOutput xss
-- Match any non space char
AnyNonSpace ~= (x : xs)
  | not $ isSpace x = yield xs
AnyNonSpace ~= _ = empty
-- Match any character including spaces
Any ~= (x : xs) =
  yield xs
Any ~= [] =
  empty
-- Match a start of line
StartOfLine ~= _ =
  undefined -- TODO
  -- Match the end of line
EndOfLine ~= (x : xs)
  | x == '\n' = yield xs
EndOfLine ~= _ =
  empty
-- Match the pattern zero or one time
Optional p ~= xss =
  p ~= xss <|> yield xss
-- Match the time zero or many times
z@(ZeroOrMore p) ~= xss =
  p ~= xss +> (z ~=)
    <|> yield xss
-- Match the time once or more times
OneOrMore p ~= xss =
  p ~= xss +> (ZeroOrMore p ~=)
-- Match the grouped sequence
Subgroup p ~= xss =
  matchRegexList p xss
-- Match either of the patterns
Alternation p1 p2 ~= xss =
  p1 ~= xss <|> p2 ~= xss
-- Match zero or many white space characters
Space ~= (x : xs)
  | isSpace x = runSpace xs
  where
    runSpace [] = yield []
    runSpace xss@(x : xs)
      | isSpace x = runSpace xs
      | otherwise = yield xss
Space ~= _ =
  empty

-- | Match a "\.\+" pattern and return it as left in the tuple
matchOutput :: String -> Maybe (String, String)
matchOutput =
  \case
    ([], _) -> empty
    x -> pure x
    . break isSpace

infixr 5 +>

-- | Monad-like transforming for usage in this module
(+>) :: Maybe ([a], b) -> (b -> Maybe ([a], b)) -> Maybe ([a], b)
x +> f = do
  (out, next) <- x
  Bf.first (++ out) <$> f next

----------------------------------------------

{- $
>>> Character 'x' ~= "xx"
Just ("","x")
>>> Character 'x' ~= "y"
Nothing

>>> Output ~= "abc d"
Just ("abc"," d")

>>> AnyNonSpace ~= "xy"
Just ("","y")
>>> AnyNonSpace ~= " x"
Nothing

>>> Any ~= "xy"
Just ("","y")
>>> Any ~= " x"
Just ("","x")

TODO StartOfLine

>>> EndOfLine ~= "\nx"
Just ("","x")
>>> EndOfLine ~= "xx"
Nothing

>>> Optional (Character 'x') ~= "xx"
Just ("","x")
>>> Optional (Character 'x') ~= "yy"
Just ("","yy")

>>> ZeroOrMore (Character 'x') ~= "xxy"
Just ("","y")
>>> ZeroOrMore (Character 'x') ~= "yy"
Just ("","yy")

>>> OneOrMore (Character 'x') ~= "xxy"
Just ("","y")
>>> OneOrMore (Character 'x') ~= "yy"
Nothing

>>> let sub = Subgroup [Character 'x', Character 'y']
>>> sub ~= "xyz"
Just ("","z")
>>> sub ~= "xxx"
Nothing

>>> let al = Alternation (Character 'a') (Character 'b')
>>> al ~= "ax"
Just ("","x")
>>> al ~= "bx"
Just ("","x")
>>> al ~= "xx"
Nothing

>>> Space ~= "   x"
Just ("","x")
>>> Space ~= "x"
Nothing
-}
