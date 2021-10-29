{-# LANGUAGE FlexibleContexts #-}

module Utilities where

import Data.Maybe ( fromJust, isJust, maybe )
import Control.Monad.Identity ( void, Identity )
import Text.Parsec.Char ( char, digit, oneOf )
import Text.Parsec.Prim ( Stream, Parsec, many, try )
import EvalError
import Text.Parsec (ParsecT)
import Data.Foldable ( Foldable(toList) )
import Control.Monad ( void )

-- | Add a pair of round braces to the String.
{-# INLINE addBrace #-}
addBrace :: String -> String
addBrace = ('(' :) . (++ ")")

-- | Apply a function if the predicate is True.
{-# INLINE applyOn #-}
applyOn :: Bool -> (a -> a) -> a -> a
applyOn p f a = if p then f a else a

-- | Ignore blankspace.
{-# INLINE eatWSP #-}
eatWSP :: Monad m => Stream s m Char => ParsecT s u m [Char]
eatWSP = many (char ' ');

-- | Parse a decimal digit.
int :: Stream s Identity Char => Parsec s u Integer
int = read <$> do
  f <- oneOf "-0123456789"
  r <- many digit
  void (try (void $ many $ char ' '))
  return (f : r)

-- | Transform a Maybe to a Either EvalError
encodeErr :: EvalError -> Maybe a -> Either EvalError a
encodeErr = flip maybe Right . Left

-- | Iterates through a Foldable with a monadic action that returns a Bool and 
-- a Maybe value. Breaks the iteration when the Bool is False. Returns a Bool
-- indicating if the iteration is completed, and the list of all Just results.
forMBreak :: Foldable t => Monad m => 
  t a -> (a -> m (Bool, Maybe b)) -> m (Bool, [b])
forMBreak xs f = go (toList xs) f
  where
    go [] _       = return (True, [])
    go (x : xs) f = do
      (b, r) <- f x 
      if b 
        then do
          (b, rs) <- go xs f
          return $ case r of
            Just r -> (b, r : rs)
            _      -> (b, rs)
        else
          return (False, [])
