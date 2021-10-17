module Expression where

-- | Type class for an expression based on recursively defined rules.
class Expression e where
  -- | Is normal (irreducible).
  isNormal :: e -> Bool

  -- | Big-Step evaluation.
  eval :: e -> Integer

  -- | Small-Step evaluation.
  eval1 :: e -> e
