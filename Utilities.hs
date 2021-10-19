module Utilities where
import Data.Maybe ( fromJust, isJust )

-- | Add a pair of round braces to the String.
{-# INLINE addBrace #-}
addBrace :: String -> String
addBrace = ('(' :) . (++ ")")

-- | Apply a function if the predicate is True.
{-# INLINE applyOn #-}
applyOn :: Bool -> (a -> a) -> a -> a
applyOn p f a = if p then f a else a
