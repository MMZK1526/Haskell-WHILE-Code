{-# LANGUAGE ConstrainedClassMethods #-}

module Expression where

import Control.Monad.Trans.State
import Control.Monad

-- | The "State" or "Context" of the expression.
type Context a = [(String, a)]

-- | Type class for an expression based on recursively defined rules.
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
  evalStar :: e -> State (Context e) [e]
  evalStar exp = do
    ctxt <- get
    case runStateT (eval1 exp) ctxt of
      Nothing     -> if isNormal exp
        then return [exp]
        -- Add a dash to represent an error state.
        -- Temporary solution; will find a more desciptive way of doing so.
        else put (("_", exp) : ctxt) >> return [exp]
      Just (e, c) -> do
        put c
        rest <- evalStar e
        return $ exp : rest

  -- | Pretty prints the result of evalStar. Only works if the expression
  -- implements 'Show'.
  evalStarPrint :: Show e => Context e -> e -> IO ()
  evalStarPrint c exp = do
    let (exps, ctxt) = runState (evalStar exp) c
    forM_ exps print
    putStrLn $ if not (null ctxt) && fst (head ctxt) == "_"
      then "Evaluation failure due to having undefined variable(s)!"
      else "Final state: " ++ show ctxt
