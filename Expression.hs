module Expression where

import Control.Monad.Trans.State ( StateT )

type Context a = [(String, a)]

-- | Type class for a pure expression based on recursively defined rules.
-- It does not have a context (state).
class Expression e where
  -- | Is normal (irreducible).
  isNormal :: e -> Bool

  -- | Big-Step evaluation.
  eval :: Context e -> e -> e

  -- | Small-Step evaluation.
  eval1 :: Context e -> StateT e Maybe (Context e)
