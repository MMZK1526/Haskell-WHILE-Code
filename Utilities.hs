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

-- | Find the first non-Nothing in a list of Maybe, together with its index.
msumMaybe :: [Maybe a] -> Maybe (Int, a)
msumMaybe = go 0
  where
    go _ [] = Nothing
    go n (x : xs)
      | isJust x  = Just (n, fromJust x)
      | otherwise = go (n + 1) xs
