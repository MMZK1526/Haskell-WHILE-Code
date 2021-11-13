{-# LANGUAGE FlexibleContexts #-}

module Utilities where

import Data.Maybe ( fromJust, isJust, maybe )
import Data.Functor.Identity
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
  if f == '-' && null r
    then fail ""
    else return (f : r)

-- | Transform a Maybe to a Either EvalError
encodeErr :: EvalError -> Maybe a -> Either EvalError a
encodeErr = flip maybe Right . Left
