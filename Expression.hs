module Expression where

-- | Type class for a pure expression based on recursively defined rules.
-- It does not have a context (state).
class PureExpression e where
  -- | Is normal (irreducible).
  isNormal :: e -> Bool

  -- | Big-Step evaluation.
  eval :: e -> Integer

  -- | Small-Step evaluation.
  eval1 :: e -> e
