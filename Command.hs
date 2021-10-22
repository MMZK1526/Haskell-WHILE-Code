{-# LANGUAGE InstanceSigs #-}

module Command where

import qualified Data.Map as M
import Control.Monad.Trans.State
import SimpleExp
import Expression
import Condition
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
  evalS :: Command -> State Context Command
  evalS lang = do
    c <- get
    case lang of
      Ret exp    -> Ret <$> evalS exp
      Skip       -> return Skip
      Asgn v exp -> do
        exp <- evalS exp
        put $ M.insert v exp c
        return Skip
      c :+: c'     -> do
        r <- evalS c
        case r of
          Ret res -> return $ Ret res
          Skip    -> evalS c'
          -- Stuck State
          _       -> return (r :+: c')
      If b c c'  -> do
        cond <- evalS b
        case cond of
          T -> evalS c
          F -> evalS c'
          -- Stuck State
          _ -> return $ If cond c c'
      While b c  -> evalS $ If b (While b c) Skip


  -- | Small-Step evaluation. Encoded with Nothing if either in normal form or
  -- stuck state.
  eval1S :: Command -> StateT Context Maybe Command
  eval1S lang = do
    c <- get
    case lang of
      Skip :+: com    -> return com   -- W-SEQ.SKIP
      com :+: com'    -> do           -- W-SEQ.LEFT
        (lang', c') <- lift $ runStateT (eval1S com) c
        put c'
        case lang' of
          Ret (Nmbr n) -> return $ Ret $ Nmbr n -- The result is found
          com''        -> return $ com'' :+: com'
      Asgn v (Nmbr n) -> do           -- W-ASS.NUM
        put $ M.insert v (Nmbr n) c
        return Skip
      Asgn v exp      -> do           -- W-ASS.EXP
        (exp', c') <- lift $ runStateT (eval1S exp) c
        put c'
        return $ Asgn v exp'
      If T com _      -> return com     -- W-COND.TRUE
      If F _ com'     -> return com'    -- W-COND.FALSE
      If b com com'   -> do             -- W-COND.BEXP
        (b', c') <- lift $ runStateT (eval1S b) c
        return $ If b' com com'
      Ret exp         -> Ret <$> eval1S exp
      _               -> lift Nothing   

comFact :: Command
comFact = Asgn "y" (EVar "x")
  :+: Asgn "a" 1 
  :+: While (CGT (EVar "y") 0) (
        Asgn "a" (Prod (EVar "a") (EVar "y"))
    :+: Asgn "y" (Mnus (EVar "y") 1)
      )
  :+: Ret (EVar "a")
