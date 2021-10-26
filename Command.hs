{-# LANGUAGE InstanceSigs #-}

module Command where

import qualified Data.Map as M
import Control.Monad.Trans.State
import SimpleExp
import Expression
import Definitions
import Control.Monad.Trans

instance Expression Command where
  -- | Is normal (irreducible).
  {-# INLINE isNormal #-}
  isNormal :: Command -> Bool
  isNormal Skip    = True
  isNormal (Ret r) = isNormal r
  isNormal _       = False

  -- | Big-Step evaluation.
  evalS :: Command -> StateT Context Maybe Command
  evalS lang = do
    c <- get
    case lang of
      Ret exp    -> Ret <$> evalS exp
      Skip       -> return Skip
      Asgn x exp -> do
        EVal v <- evalS exp
        put $ M.insert x v c
        return Skip
      c :+: c'     -> do
        r <- evalS c
        case r of
          Ret res -> return $ Ret res
          Skip    -> evalS c'
          -- Stuck State
          _       -> return (r :+: c')
      If b c c'  -> do
        EVal cond <- evalS b
        case cond of
          VBool True  -> evalS c
          VBool False -> evalS c'
          -- Stuck State
          _ -> lift Nothing
      While b c  -> evalS $ If b (c :+: While b c) Skip

  -- | Small-Step evaluation. Encoded with Nothing if either in normal form or
  -- stuck state.
  eval1S :: Command -> StateT Context Maybe Command
  eval1S lang = do
    c <- get
    case lang of
      Skip :+: com -> return com    -- W-SEQ.SKIP
      com :+: com' -> do            -- W-SEQ.LEFT
        lang' <- eval1S com  
        case lang' of
          Ret (EVal v) -> return $ Ret $ EVal v
          com''        -> return $ com'' :+: com'
      Asgn x (EVal v) -> do         -- W-ASS.NUM
        put $ M.insert x v c
        return Skip
      Asgn x exp -> do              -- W-ASS.EXP
        exp' <- eval1S exp
        return $ Asgn x exp'
      If (EVal (VBool True)) com _  -- W-COND.TRUE
        -> return com    
      If (EVal (VBool False)) _ com -- W-COND.FALSE     
        -> return com  
      If b com com' -> do          -- W-COND.BEXP
        b' <- eval1S b
        return $ If b' com com'
      Ret exp    -> Ret <$> eval1S exp
      While b c  -> return $       -- W-WHILE
        If b (c :+: While b c) Skip
      _               -> lift Nothing   

comFact :: Command
comFact = 
      Asgn "y" (EVar "x")
  :+: Asgn "a" 1 
  :+: While (EGT (EVar "y") 0) (
        Asgn "a" (Prod (EVar "a") (EVar "y"))
    :+: Asgn "y" (Mnus (EVar "y") 1)
      )
  :+: Ret (EVar "a")
