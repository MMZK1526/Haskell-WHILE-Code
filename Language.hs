{-# LANGUAGE InstanceSigs #-}

module Language where

import SimpleExp
import Expression
import Definitions ( Context, Language(Exp) )

-- instance Expression Language where
  
--   {-# INLINE isNormal #-}
--   isNormal :: Language -> Bool
--   isNormal = undefined

--   -- | Big-Step evaluation.
--   evalS :: Context -> Language -> Language
--   evalS c (Exp exp)          = Exp $ evalS c exp
