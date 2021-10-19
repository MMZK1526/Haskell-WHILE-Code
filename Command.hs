{-# LANGUAGE InstanceSigs #-}

module Command where

import qualified Data.Map as M
import Control.Monad.Trans.State
import SimpleExp
import Expression
import Definitions
import Control.Monad.Trans

instance Expression Command where
  
  {-# INLINE isNormal #-}
  isNormal :: Command -> Bool
  isNormal Skip    = True
  isNormal (Ret r) = isNormal r
  isNormal _       = False

  -- | Big-Step evaluation.
  evalS :: Command -> State Context Command
  evalS lang = do
    c <- get
    case lang of
      Ret exp      -> Ret <$> evalS exp
      Skip         -> return Skip -- B-SKIP
      (Asgn v exp) -> do          -- B-ASS
        exp <- evalS exp
        put $ M.insert v exp c
        return Skip
      (c :+: c')   -> do          -- B-SEQ
        r <- evalS c
        case r of
          Ret res -> return $ Ret res
          Skip    -> evalS c'
          _       -> return (r :+: c')

  -- | Small-Step evaluation. Encoded with Nothing if either in normal form or
  -- wrong state.
  eval1S :: Command -> StateT Context Maybe Command
  eval1S lang = do
    c <- get
    case lang of
      (Skip :+: com)    -> return com   -- W-SEQ.SKIP
      (com :+: com')    -> do           -- W-SEQ.LEFT
        (lang', c') <- lift $ runStateT (eval1S com) c
        put c'
        case lang' of
          Ret (Nmbr n) -> return $ Ret $ Nmbr n -- The result is found
          com''        -> return $ com'' :+: com'
      (Asgn v (Nmbr n)) -> do           -- W-ASS.NUM
        put $ M.insert v (Nmbr n) c
        return Skip
      (Asgn v exp)      -> do           -- W-ASS.EXP
        (exp', c') <- lift $ runStateT (eval1S exp) c
        put c'
        return $ Asgn v exp'
      Ret exp           -> Ret <$> eval1S exp
      _                 -> lift Nothing   
