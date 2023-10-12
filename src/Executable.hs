{-# LANGUAGE ConstrainedClassMethods #-}

module Executable where

import qualified Data.Map as M
import Control.Monad.Trans.State
import Control.Monad
import Definitions
import EvalError

-- | Type class for an executable based on recursively defined rules.
class Executable e where
  -- | Is normal (irreducible).
  isNormal :: e -> Bool

  -- | Big-Step evaluation. Encoded with an error if cannot reach normal state.
  evalS :: e -> StateT Context (Either EvalError) e

  -- | Small-Step evaluation. Encoded with an error if either in normal form or
  -- stuck state.
  eval1S :: e -> StateT Context (Either EvalError) e

  -- | Big-Step evaluation, starting from an empty state, discarding the state.
  eval :: e -> Either EvalError e
  eval exp = evalStateT (evalS exp) emptyContext

  -- | Big-Step evaluation, discarding the state.
  evalS' :: Context -> e -> Either EvalError e
  evalS' c exp = evalStateT (evalS exp) c

  -- | Return a list of Expressions, each one is one-step reduced from the
  -- previous one. Starting from an empty state, discarding the state.
  evalStar :: e -> ([e], String)
  evalStar e = evalState (evalStarS e) emptyContext

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
  evalStarPrint = evalStarPrintS emptyContext

  -- | Pretty prints the result of evalStarS. Only works if the expression
  -- implements 'Show'.
  evalStarPrintS :: Show e => Context -> e -> IO ()
  evalStarPrintS c exp = do
    let ((exps, msg), ctxt) = runState (evalStarS exp) c
    forM_ (zip [0..] exps) $
      \(i, exp) -> putStrLn $ "Step " ++ show i ++ ":\n" ++ show exp ++ "\n"
    putStrLn $ if msg == show NormalFormError
      then show ctxt
      else msg ++ "\nContext: " ++ show ctxt
