{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
-- for MonadFail (Either String)
{-# LANGUAGE FlexibleInstances #-}

module Text.Parser
  (
    -- * Base parser
    Parser(runParser),
    parse,
    pfail,
    -- * Basic combinators
    satisfy,
    succeed,
    epsilon,
    choice,
    chain,
    -- * Specialized parser for strings
    CharParser,
    char,
    string,
    -- * Operators
    (=?>),
    (=$>),
  )
where

import Control.Applicative
    ( Applicative(liftA2), Alternative(empty, (<|>)) )
import Control.Monad ((>=>))
import qualified Data.Bifunctor as Bf
import Data.Foldable (asum)

-- | Parser type
newtype Parser m i o = MkParser {runParser :: MonadFail m => [i] -> m (o, [i])}

{-
>>>
>>> y = 20
>>> x+ y
30

-}

-- | Run a parser
parse :: MonadFail m => Parser m i o -> [i] -> m o
parse p = runParser p >=> validate
  where
    validate (out, []) = pure out
    validate _ = fail "Input not fully consumed"

------------------------------------------
-- Class instances

instance Functor (Parser m i) where
  fmap f (MkParser x) = MkParser (fmap (Bf.first f) . x)

instance Applicative (Parser m i) where
  pure f = MkParser $ pure . (f,)
  (MkParser f) <*> (MkParser g) =
    MkParser $ \str1 -> do
      (x, str2) <- f str1
      (y, str3) <- g str2
      return (x y, str3)

instance Alternative m => Alternative (Parser m i) where
  empty = MkParser $ const (fail "Empty parser")
  (MkParser l) <|> (MkParser r) =
    MkParser $ \str -> l str <|> r str

instance Monad (Parser m i) where
  (MkParser f) >>= g =
    MkParser $ \inp -> do
      (x, str2) <- f inp
      let MkParser h = g x
      h str2

{-
>>> parse (char 'x' >>= \y -> string ['l', y] ) $ "xlx"
"lx"

-}

instance MonadFail (Parser m i) where
  fail str = MkParser (const $ fail str)

pfail :: String -> Parser m i o
pfail = fail

------------------------------------------
-- | Put a token to predicate, returning it on true and failing on false
satisfy :: (i -> Bool) -> Parser m i i
satisfy p = MkParser $ \case
  (x : xs) | p x -> pure (x, xs)
  _ -> fail "Predicate not satified"

-- | Satisfy that returns a different output
(=?>) :: (i -> Bool) -> a -> Parser m i a
(=?>) p = (<$ satisfy p)

-- | Always get the next token
succeed :: Parser m i i
succeed = satisfy (const True)

-- | Don't take anything from input and succeed
epsilon :: Parser m i ()
epsilon = pure ()

-- | Try to succeed with any of the given parsers
choice :: Alternative m => [Parser m i o] -> Parser m i o
choice = asum


-- | Chain many parsers together
chain :: [Parser m i o] -> Parser m i [o]
chain = foldr (liftA2 (:)) (pure [])

--------------------------------------------

-- | Parser for Strings
type CharParser m o = Parser m Char o

-- | Try to match a certain character
char :: Char -> CharParser m Char
char ch = satisfy (== ch)

{-
>>> parse (string "hello") "hello"
"hello"

-}

-- | Match a certain string
string :: String -> CharParser m String
string = chain . map char

-- | Match a certain string, returning a different token
(=$>) :: String -> a -> CharParser m a
(=$>) str = (<$ string str)


--------------------------------------------
-- Helper MonadFail instance
instance MonadFail (Either String) where
  fail = Left
