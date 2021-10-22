{-# LANGUAGE InstanceSigs #-}

module Condition where

import Expression
import SimpleExp
import Definitions
import Control.Monad.Trans.State
import Control.Monad
import Control.Monad.Trans

instance Expression Condition where
  -- | Is normal (irreducible).
  isNormal :: Condition -> Bool
  isNormal T = True
  isNormal F = True
  isNormal _ = False

  -- | Big-Step evaluation.
  evalS :: Condition -> State Context Condition
  evalS con = do
    c <- get
    case con of
      T                      -> return T
      F                      -> return F
      Not T                  -> return F
      Not F                  -> return T
      Not con                -> Not <$> evalS con
      And T        con       -> evalS con
      And F        _         -> return F
      And con      con'      -> liftM2 And (evalS con) (evalS con') >>= evalS
      Or  T        con       -> return T
      Or  F        con       -> evalS con
      Or  con      con'      -> liftM2 Or  (evalS con) (evalS con') >>= evalS
      CLT (Nmbr n) (Nmbr n') -> return $ if n < n' then T else F
      CLT exp exp'           -> do
        l <- evalS exp
        r <- evalS exp'
        if isNmbr l && isNmbr r
          then return $ if fromNmbr l < fromNmbr r then T else F
          else return $ CLT l r
      CGT (Nmbr n) (Nmbr n') -> return $ if n > n' then T else F
      CGT exp exp'           -> do
        l <- evalS exp
        r <- evalS exp'
        if isNmbr l && isNmbr r
          then return $ if fromNmbr l > fromNmbr r then T else F
          else return $ CGT l r
      CEQ (Nmbr n) (Nmbr n') -> return $ if n == n' then T else F
      CEQ exp exp'           -> do
        l <- evalS exp
        r <- evalS exp'
        if isNmbr l && isNmbr r
          then return $ if fromNmbr l == fromNmbr r then T else F
          else return $ CEQ l r
      CLE (Nmbr n) (Nmbr n') -> return $ if n <= n' then T else F
      CLE exp exp'           -> do
        l <- evalS exp
        r <- evalS exp'
        if isNmbr l && isNmbr r
          then return $ if fromNmbr l <= fromNmbr r then T else F
          else return $ CLE l r
      CNE (Nmbr n) (Nmbr n') -> return $ if n /= n' then T else F
      CNE exp exp'           -> do
        l <- evalS exp
        r <- evalS exp'
        if isNmbr l && isNmbr r
          then return $ if fromNmbr l /= fromNmbr r then T else F
          else return $ CNE l r
      CGE (Nmbr n) (Nmbr n') -> return $ if n >= n' then T else F
      CGE exp exp'           -> do
        l <- evalS exp
        r <- evalS exp'
        if isNmbr l && isNmbr r
          then return $ if fromNmbr l >= fromNmbr r then T else F
          else return $ CGE l r

  -- | Small-Step evaluation. Encoded with Nothing if either in normal form or
  -- stuck state.
  eval1S :: Condition -> StateT Context Maybe Condition
  eval1S con = case con of
    Not T                  -> return F
    Not F                  -> return T
    Not con                -> Not <$> eval1S con
    And T        con       -> return con
    And F        _         -> return F
    And con      con'      -> liftM2 And (eval1S con) (return con')
    Or  T        con       -> return T
    Or  F        con       -> return con
    Or  con      con'      -> liftM2 Or  (eval1S con) (return con')
    CLT (Nmbr n) (Nmbr n') -> return $ if n < n' then T else F
    CLT (Nmbr n) exp'      -> CLT (Nmbr n) <$> eval1S exp'
    CLT exp exp'           -> liftM2 CLT (eval1S exp) (return exp')
    CGT (Nmbr n) (Nmbr n') -> return $ if n > n' then T else F
    CGT (Nmbr n) exp'      -> CGT (Nmbr n) <$> eval1S exp'
    CGT exp exp'           -> liftM2 CGT (eval1S exp) (return exp')
    CEQ (Nmbr n) (Nmbr n') -> return $ if n == n' then T else F
    CEQ (Nmbr n) exp'      -> CEQ (Nmbr n) <$> eval1S exp'
    CEQ exp exp'           -> liftM2 CEQ (eval1S exp) (return exp')
    CLE (Nmbr n) (Nmbr n') -> return $ if n <= n' then T else F
    CLE (Nmbr n) exp'      -> CLE (Nmbr n) <$> eval1S exp'
    CLE exp exp'           -> liftM2 CLE (eval1S exp) (return exp')
    CNE (Nmbr n) (Nmbr n') -> return $ if n /= n' then T else F
    CNE (Nmbr n) exp'      -> CNE (Nmbr n) <$> eval1S exp'
    CNE exp exp'           -> liftM2 CNE (eval1S exp) (return exp')
    CGE (Nmbr n) (Nmbr n') -> return $ if n >= n' then T else F
    CGE (Nmbr n) exp'      -> CGE (Nmbr n) <$> eval1S exp'
    CGE exp exp'           -> liftM2 CGE (eval1S exp) (return exp')
    _                      -> lift Nothing
