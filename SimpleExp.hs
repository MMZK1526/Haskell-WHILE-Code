{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimpleExp where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Expression ( Expression(..) )
import Definitions
import Data.Maybe
import Control.Monad.Trans.Maybe

{-# INLINE fromNmbr #-}
fromNmbr :: SimpleExp -> Integer
fromNmbr (Nmbr n) = n
fromNmbr _        = error "Cannot extract from a non-value!"

instance Num SimpleExp where
  x + y       = Plus x y
  x * y       = Prod x y
  fromInteger = Nmbr
  abs         = undefined
  signum      = undefined
  negate      = undefined

instance Expression SimpleExp where
  -- | Is normal (irreducible).
  {-# INLINE isNormal #-}
  isNormal :: SimpleExp -> Bool
  isNormal = isNmbr

  -- | Big-Step evaluation.
  eval :: Context -> SimpleExp -> SimpleExp
  eval _ (Nmbr n)    = Nmbr n -- B-Num
  eval c (Plus e e')          -- B-Add
    | isNmbr l && isNmbr r = Nmbr $ fromNmbr l + fromNmbr r
    | otherwise            = l + r
    where
      l = eval c e
      r = eval c e'
  eval c (Prod e e')          -- B-Mul
    | isNmbr l && isNmbr r = Nmbr $ fromNmbr l * fromNmbr r
    | otherwise            = l * r
    where
      l = eval c e
      r = eval c e'
  eval c (EVar v) = maybe (EVar v) (eval c) (lookup v c)

  -- | Small-Step evaluation. Encoded with Nothing if either in normal form or
  -- wrong state.
  eval1 :: SimpleExp -> StateT Context Maybe SimpleExp
  eval1 exp = do
    c <- get
    case exp of
      Plus e e' -> case (e, e') of
        (Nmbr n, Nmbr n') -> return (Nmbr $ n + n') -- W-EXP.ADD
        (Nmbr n, e')      -> do                     -- W-EXP.RIGHT
          (e'', c') <- lift $ runStateT (eval1 e') c
          put c' >> return (Plus (Nmbr n) e'')
        (e,      e')      -> do                     -- W-EXP.LEFT
          (e'', c') <- lift $ runStateT (eval1 e) c
          put c' >> return (Plus e'' e')
      Prod e e' -> case (e, e') of
        (Nmbr n, Nmbr n') -> return (Nmbr $ n * n') -- W-EXP.MUL
        (Nmbr n, e')      -> do                     -- W-EXP.RIGHT
          (e'', c') <- lift $ runStateT (eval1 e') c
          put c' >> return (Prod (Nmbr n) e'')
        (e,      e')      -> do                     -- W-EXP.LEFT
          (e'', c') <- lift $ runStateT (eval1 e) c
          put c' >> return (Prod e'' e')
      EVar v    -> do
        let e' = lookup v c
        maybe (lift Nothing) return e'
      Nmbr n    -> lift Nothing
