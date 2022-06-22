{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Text.Parser
  ( -- * Base parser
    Parser (runParser),
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
  ( Alternative (empty, (<|>)),
    Applicative (liftA2),
  )
import Control.Monad ((>=>))
import qualified Data.Bifunctor as Bf
import Data.Foldable (asum)

-- | Base parser type. It takes a MonadFail `m`, input type `i` and output type `o`
newtype Parser m i o = MkParser {runParser :: MonadFail m => [i] -> m (o, [i])}

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

-- $
-- Monad test
-- >>> parse (char 'a' >>= \a -> string ['b', a] ) $ "aba"
-- "ba"

instance Monad (Parser m i) where
  (MkParser f) >>= g =
    MkParser $ \inp -> do
      (x, str2) <- f inp
      let MkParser h = g x
      h str2

instance MonadFail (Parser m i) where
  fail str = MkParser (const $ fail str)

-- | Specialization of MonadFail fail for parsers
pfail :: String -> Parser m i o
pfail = fail

------------------------------------------
-- $setup
-- >>> import Data.Char (isSpace)

-- $
-- >>> parse (satisfy isSpace) " "
-- ' '

-- | Put a token to predicate, returning it on true and failing on false
satisfy :: (i -> Bool) -> Parser m i i
satisfy p = MkParser $ \case
  (x : xs) | p x -> pure (x, xs)
  _ -> fail "Predicate not satified"

-- $
-- >>> parse (isSpace =?> "output") " "
-- "output"

-- | Satisfy that returns a different output
(=?>) :: (i -> Bool) -> a -> Parser m i a
(=?>) p = (<$ satisfy p)

-- $
-- >>> parse succeed  "c"
-- 'c'

-- | Always get the next token
succeed :: Parser m i i
succeed = satisfy (const True)

-- $
-- >>> const 'x' <$> parse epsilon ""
-- 'x'

-- | Don't take anything from input and succeed
epsilon :: Parser m i ()
epsilon = pure ()


-- $
-- >>> let p = choice [isSpace =?> 'x', (== 'c') =?> 'y']
-- >>> parse p " "
-- 'x'
-- >>> parse p "c"
-- 'y'

-- | Try to succeed with any of the given parsers
choice :: Alternative m => [Parser m i o] -> Parser m i o
choice = asum

-- $
-- >>> let p x = satisfy (== x)
-- >>> parse (chain [p 10, p 20, p (-5)]) [10, 20, -5]
-- [10,20,-5]

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
