{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimpleExp where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Expression ( Context, Expression(..) )
import Utilities ( addBrace, applyOn )
import Data.Maybe
import Control.Monad.Trans.Maybe

-- | Simple Expression:
-- E ::= n | E + E | E * E
data SimpleExp
  = Nmbr { num :: Integer }
  | Plus { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | Prod { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | EVar {var :: String}
  deriving (Eq)

{-# INLINE isNmbr #-}
{-# INLINE isPlus #-}
{-# INLINE isProd #-}
{-# INLINE isEVar #-}
isNmbr, isPlus, isProd, isEVar :: SimpleExp -> Bool
isNmbr Nmbr {} = True
isNmbr _       = False
isPlus Plus {} = True
isPlus _       = False
isProd Prod {} = True
isProd _       = False
isEVar EVar {} = True
isEVar _       = False

{-# INLINE fromNmbr #-}
fromNmbr :: SimpleExp -> Integer
fromNmbr (Nmbr n) = n
fromNmbr _        = error "Cannot extract from a non-value!"

instance Show SimpleExp where
  show exp = "Exp: " ++ show' exp
    where
      show' (Nmbr n) = show n
      show' (EVar v) = v
      show' (Plus e e')
        = show' e ++
          " + " ++
          applyOn (isPlus e') addBrace (show' e')
      show' (Prod e e')
        = applyOn (isPlus e) addBrace (show' e) ++
          " * " ++
          applyOn (not (isNmbr e' || isEVar e')) addBrace (show' e')

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
  eval :: Context SimpleExp -> SimpleExp -> SimpleExp
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

  -- | Small-Step evaluation.
  eval1 :: SimpleExp -> StateT (Context SimpleExp) Maybe SimpleExp
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
