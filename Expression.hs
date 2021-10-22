{-# LANGUAGE ConstrainedClassMethods #-}

module Expression where

import qualified Data.Map as M
import Control.Monad.Trans.State
import Control.Monad
import Definitions ( Context )

-- | Type class for an expression based on recursively defined rules.
class Expression e where
  -- | Is normal (irreducible).
  isNormal :: e -> Bool

  -- | Big-Step evaluation.
  evalS :: e -> State Context e

  -- | Small-Step evaluation. Encoded with Nothing if either in normal form or
  -- stuck state.
  eval1S :: e -> StateT Context Maybe e

  -- | Big-Step evaluation, starting from an empty state, discarding the state.
  eval :: e -> e
  eval exp = evalState (evalS exp) M.empty

  -- | Return a list of Expressions, each one is one-step reduced from the 
  -- previous one. Starting from an empty state, discarding the state.
  evalStar :: e -> [e]
  evalStar e = evalState (evalStarS e) M.empty

  -- | Return a list of Expressions, each one is one-step reduced from the 
  -- previous one.
  evalStarS :: e -> State Context [e]
  evalStarS exp = do
    ctxt <- get
    case runStateT (eval1S exp) ctxt of
      Nothing     -> if isNormal exp
        then return [exp]
        -- Add a dash to represent an error state.
        -- Temporary solution; will find a more desciptive way of doing so.
        else put (M.insert "_" 0 ctxt) >> return [exp]
      Just (e, c) -> do
        put c
        rest <- evalStarS e
        return $ exp : rest

  -- | Pretty prints the result of evalStar, starting from an empty state. 
  -- Only works if the expression implements 'Show'.
  evalStarPrint :: Show e => e -> IO ()
  evalStarPrint = evalStarPrintS M.empty

  -- | Pretty prints the result of evalStar. Only works if the expression
  -- implements 'Show'.
  evalStarPrintS :: Show e => Context -> e -> IO ()
  evalStarPrintS c exp = do
    let (exps, ctxt) = runState (evalStarS exp) c
    forM_ (zip [0..] exps) $ 
        \(i, exp) -> putStrLn $ "Step " ++ show i ++ ":\n" ++ show exp ++ "\n"
    putStrLn $ if not (null ctxt) && M.member "_" ctxt
      then "Evaluation failed due to having undefined variable(s)!" ++ show (M.toList ctxt)
      else "Final state: " ++ show (M.toList ctxt)
