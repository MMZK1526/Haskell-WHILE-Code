{-# LANGUAGE InstanceSigs #-}

module Language where

import Control.Monad.Trans.State
import SimpleExp
import Expression
import Definitions

instance Expression Language where
  
  {-# INLINE isNormal #-}
  isNormal :: Language -> Bool
  isNormal = undefined

  -- | Big-Step evaluation.
  -- evalS :: Language -> State Context Language
  -- evalS lang = do
  --   c <- get
  --   case lang of
  --     Exp exp          -> Exp <$> evalS exp -- B-EXP
  --     Com Skip         -> return $ Com Skip -- B-SEQ.SKIP
  --     Com (Asgn v exp) -> do                -- B-ASS
  --       exp <- evalS exp
  --       put $ (v, exp) : c
  --       return $ Com Skip 
