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
      CLT exp exp'           -> liftM2 CLT (evalS exp) (evalS exp') >>= evalS
      CGT (Nmbr n) (Nmbr n') -> return $ if n > n' then T else F
      CGT exp exp'           -> liftM2 CGT (evalS exp) (evalS exp') >>= evalS
      CEQ (Nmbr n) (Nmbr n') -> return $ if n == n' then T else F
      CEQ exp exp'           -> liftM2 CEQ (evalS exp) (evalS exp') >>= evalS
      CLE (Nmbr n) (Nmbr n') -> return $ if n <= n' then T else F
      CLE exp exp'           -> liftM2 CLE (evalS exp) (evalS exp') >>= evalS
      CNE (Nmbr n) (Nmbr n') -> return $ if n /= n' then T else F
      CNE exp exp'           -> liftM2 CNE (evalS exp) (evalS exp') >>= evalS
      CGE (Nmbr n) (Nmbr n') -> return $ if n >= n' then T else F
      CGE exp exp'           -> liftM2 CGE (evalS exp) (evalS exp') >>= evalS

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
