{-# LANGUAGE InstanceSigs #-}

module Language where

import SimpleExp
import Expression
import Definitions ( Context, Language(Exp) )

instance Expression Language where
  
  {-# INLINE isNormal #-}
  isNormal :: Language -> Bool
  isNormal = undefined

  -- | Big-Step evaluation.
  eval :: Context -> Language -> Language
  eval c (Exp exp)          = Exp $ eval c exp
