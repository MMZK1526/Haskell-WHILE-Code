{-# LANGUAGE ConstrainedClassMethods #-}

module Expression where

import qualified Data.Map as M
import Control.Monad.Trans.State
import Control.Monad
import Definitions
import EvalError

-- | Type class for an expression based on recursively defined rules.
class Expression e where
  -- | Is normal (irreducible).
  isNormal :: e -> Bool

  -- | Big-Step evaluation. Encoded with Nothing if cannot reach normal state.
  evalS :: e -> StateT Context Maybe e
  
  -- | Small-Step evaluation. Encoded with an error if either in normal form or
  -- stuck state.
  eval1S :: e -> StateT Context (Either EvalError) e

  -- | Big-Step evaluation, starting from an empty state, discarding the state.
  eval :: e -> Maybe e
  eval exp = evalStateT (evalS exp) M.empty

  -- | Return a list of Expressions, each one is one-step reduced from the
  -- previous one. Starting from an empty state, discarding the state.
  evalStar :: e -> ([e], String)
  evalStar e = evalState (evalStarS e) M.empty

  -- | Return a list of Expressions, each one is one-step reduced from the
  -- previous one.
  evalStarS :: e -> State Context ([e], String)
  evalStarS exp = do
    ctxt <- get
    case runStateT (eval1S exp) ctxt of
      Left str     -> return ([exp], show str)
      Right (e, c) -> do
        put c
        (rest, str) <- evalStarS e
        return (exp : rest, str)

  -- | Pretty prints the result of evalStar, starting from an empty state.
  -- Only works if the expression implements 'Show'.
  evalStarPrint :: Show e => e -> IO ()
  evalStarPrint = evalStarPrintS M.empty

  -- | Pretty prints the result of evalStar. Only works if the expression
  -- implements 'Show'.
  evalStarPrintS :: Show e => Context -> e -> IO ()
  evalStarPrintS c exp = do
    let ((exps, msg), ctxt) = runState (evalStarS exp) c
    forM_ (zip [0..] exps) $
        \(i, exp) -> putStrLn $ "Step " ++ show i ++ ":\n" ++ show exp ++ "\n"
    putStrLn $ if msg == show NormalFormError
      then "Final state: " ++ show (M.toList ctxt)
      else msg
