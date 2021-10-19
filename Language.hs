{-# LANGUAGE InstanceSigs #-}

module Language where

import qualified Data.Map as M
import Control.Monad.Trans.State
import SimpleExp
import Expression
import Definitions
import Control.Monad.Trans

instance Expression Language where
  
  {-# INLINE isNormal #-}
  isNormal :: Language -> Bool
  isNormal (Exp exp)  = isNormal exp
  isNormal (Com Skip) = True
  isNormal _          = False

  -- | Big-Step evaluation.
  evalS :: Language -> State Context Language
  evalS lang = do
    c <- get
    case lang of
      Exp exp          -> Exp <$> evalS exp -- B-EXP
      Com Skip         -> return $ Com Skip -- B-SKIP
      Com (Asgn v exp) -> do                -- B-ASS
        exp <- evalS exp
        put $ M.insert v exp c
        return $ Exp exp
      Com (c :+: c')   -> do                -- B-SEQ
        evalS $ Com c -- The result (if any) is discarded
        evalS $ Com c'

  -- | Small-Step evaluation. Encoded with Nothing if either in normal form or
  -- wrong state.
  eval1S :: Language -> StateT Context Maybe Language
  eval1S lang = do
    c <- get
    case lang of
      Exp exp               -> Exp <$> eval1S exp -- W-EXP
      Com (Skip :+: com)    -> return $ Com com   -- W-SEQ.SKIP
      Com (com :+: com')    -> do                 -- W-SEQ.LEFT
        (lang', c') <- lift $ runStateT (eval1S $ Com com) c
        put c'
        case lang' of
          Com com'' -> return $ Com $ com'' :+: com'
          Exp _     -> return $ Com com' -- The result is discarded
      Com (Asgn v (Nmbr n)) -> do                 -- W-ASS.NUM
        put $ M.insert v (Nmbr n) c
        return $ Com Skip
      Com (Asgn v exp)      -> do                 -- W-ASS.EXP
        (exp', c') <- lift $ runStateT (eval1S exp) c
        put c'
        return $ Com $ Asgn v exp'    
      Com _                 -> lift Nothing   
