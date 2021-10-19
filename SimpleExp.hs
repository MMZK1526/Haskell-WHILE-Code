{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimpleExp where

import qualified Data.Map as M
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

instance Expression SimpleExp where
  -- | Is normal (irreducible).
  {-# INLINE isNormal #-}
  isNormal :: SimpleExp -> Bool
  isNormal = isNmbr

  -- | Big-Step evaluation.
  evalS :: SimpleExp -> State Context SimpleExp
  evalS exp = do
    c <- get
    case exp of
      Nmbr n    -> return $ Nmbr n -- B-Num
      Plus e e' -> do              -- B-Add
        l <- evalS e
        r <- evalS e'
        if isNmbr l && isNmbr r
          then return $ Nmbr $ fromNmbr l + fromNmbr r
          else return $ l + r
      Prod e e' -> do              -- B-Mul
        l <- evalS e
        r <- evalS e'
        if isNmbr l && isNmbr r
          then return $ Nmbr $ fromNmbr l * fromNmbr r
          else return $ l * r
      EVar v    -> do
        let mv = M.lookup v c
        case mv of
          Nothing -> return $ EVar v
          Just e  -> return e

  -- | Small-Step evaluation. Encoded with Nothing if either in normal form or
  -- wrong state.
  eval1S :: SimpleExp -> StateT Context Maybe SimpleExp
  eval1S exp = do
    c <- get
    case exp of
      Plus e e' -> case (e, e') of
        (Nmbr n, Nmbr n') -> return (Nmbr $ n + n') -- W-EXP.ADD
        (Nmbr n, e')      -> do                     -- W-EXP.RIGHT
          (e'', c') <- lift $ runStateT (eval1S e') c
          put c' >> return (Plus (Nmbr n) e'')
        (e,      e')      -> do                     -- W-EXP.LEFT
          (e'', c') <- lift $ runStateT (eval1S e) c
          put c' >> return (Plus e'' e')
      Prod e e' -> case (e, e') of
        (Nmbr n, Nmbr n') -> return (Nmbr $ n * n') -- W-EXP.MUL
        (Nmbr n, e')      -> do                     -- W-EXP.RIGHT
          (e'', c') <- lift $ runStateT (eval1S e') c
          put c' >> return (Prod (Nmbr n) e'')
        (e,      e')      -> do                     -- W-EXP.LEFT
          (e'', c') <- lift $ runStateT (eval1S e) c
          put c' >> return (Prod e'' e')
      EVar v    -> do
        let e' = M.lookup v c
        maybe (lift Nothing) return e'
      Nmbr n    -> lift Nothing
