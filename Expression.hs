module Expression where

import Control.Monad.Trans.State

type Context a = [(String, a)]

-- | Type class for a pure expression based on recursively defined rules.
-- It does not have a context (state).
class Expression e where
  -- | Is normal (irreducible).
  isNormal :: e -> Bool

  -- | Big-Step evaluation.
  eval :: Context e -> e -> e

  -- | Small-Step evaluation. Encoded with Nothing if either in normal form or
  -- wrong state.
  eval1 :: e -> StateT (Context e) Maybe e

  -- | Return a list of Expressions, each one is one-step reduced from the 
  -- previous one.
  -- evalStar :: Context e -> State [e] (Context e)
  -- evalStar c = do
  --   let cur = runStateT (eval1 c)
  --   rest <- evalStar c
  --   put (curr : rest)
