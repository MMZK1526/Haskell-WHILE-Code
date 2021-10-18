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
  -- wrong state.
  eval1S :: e -> StateT Context Maybe e

  -- | Big-Step evaluation, discarding the state.
  eval :: Context -> e -> e
  eval c exp = evalState (evalS exp) c

  -- | Return a list of Expressions, each one is one-step reduced from the 
  -- previous one.
  evalStar :: e -> State Context [e]
  evalStar exp = do
    ctxt <- get
    case runStateT (eval1S exp) ctxt of
      Nothing     -> if isNormal exp
        then return [exp]
        -- Add a dash to represent an error state.
        -- Temporary solution; will find a more desciptive way of doing so.
        else put (M.insert "_" undefined ctxt) >> return [exp]
      Just (e, c) -> do
        put c
        rest <- evalStar e
        return $ exp : rest

  -- | Pretty prints the result of evalStar. Only works if the expression
  -- implements 'Show'.
  evalStarPrint :: Show e => Context -> e -> IO ()
  evalStarPrint c exp = do
    let (exps, ctxt) = runState (evalStar exp) c
    forM_ exps print
    putStrLn $ if not (null ctxt) && M.member "_" ctxt
      then "Evaluation failure due to having undefined variable(s)!"
      else "Final state: " ++ show (M.toList ctxt)
