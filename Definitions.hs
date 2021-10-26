module Definitions where

import Data.Map ( Map )
import Utilities ( addBrace, applyOn )

-- | Simple Expression:
-- E ::= v | n | E + E | E - E | E * E
data SimpleExp
  = EVar {var :: String}
  | Nmbr { num :: Integer }
  | Plus { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | Mnus { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | Prod { exp1 :: SimpleExp, exp2 :: SimpleExp }
  deriving (Eq)

{-# INLINE isNmbr #-}
{-# INLINE isPlus #-}
{-# INLINE isMnus #-}
{-# INLINE isProd #-}
{-# INLINE isEVar #-}
isNmbr, isPlus, isMnus, isProd, isEVar :: SimpleExp -> Bool
isNmbr Nmbr {} = True
isNmbr _       = False
isPlus Plus {} = True
isPlus _       = False
isMnus Mnus {} = True
isMnus _       = False
isProd Prod {} = True
isProd _       = False
isEVar EVar {} = True
isEVar _       = False

instance Show SimpleExp where
  show (Nmbr n) = show n
  show (EVar v) = v
  show (Plus e e')
    = show e ++
      " + " ++
      applyOn (isPlus e' || isMnus e') addBrace (show e')
  show (Mnus e e')
    = show e ++
      " - " ++
      applyOn (isPlus e' || isMnus e') addBrace (show e')
  show (Prod e e')
    = applyOn (isPlus e || isMnus e) addBrace (show e) ++
      " * " ++
      applyOn (not (isNmbr e' || isEVar e')) addBrace (show e')

instance Num SimpleExp where
  x + y       = Plus x y
  x * y       = Prod x y
  fromInteger = Nmbr
  abs         = undefined
  signum      = undefined
  negate      = undefined

-- | Condition:
-- B ::= "true" | "false" | "not" B | B "and" B | B "or" B 
-- | E < E | E = E | E > E | E <= E | E != E | E >= E
data Condition
  = T
  | F
  | Not Condition
  | And Condition Condition
  | Or  Condition Condition
  | CLT SimpleExp SimpleExp
  | CGT SimpleExp SimpleExp
  | CEQ SimpleExp SimpleExp
  | CLE SimpleExp SimpleExp
  | CNE SimpleExp SimpleExp
  | CGE SimpleExp SimpleExp
  deriving Eq

instance Show Condition where
  show T          = "true"
  show F          = "false"
  show (Not b)    = '!' : show b
  show (And b b') = show b ++ " & "  ++ show b'
  show (Or  b b') = show b ++ " | "  ++ show b'
  show (CLT e e') = show e ++ " < "  ++ show e'
  show (CGT e e') = show e ++ " > "  ++ show e'
  show (CEQ e e') = show e ++ " = "  ++ show e'
  show (CLE e e') = show e ++ " <= " ++ show e'
  show (CNE e e') = show e ++ " >= " ++ show e'
  show (CGE e e') = show e ++ " != " ++ show e'

-- | This is our entire language.
-- Note here we have 2 kinds of answer configuration, namely "skip" and 
-- "return" E (if E is in normal form as well).
-- This is different to what's introduced in the course (where there is no
-- "return"), but with "return" we can see the evaluated result without looking
-- into the state.
-- Command:
-- C ::= v = E | C; C | "skip" | "return" E | "if" B "then" C "else" C
-- | "while" B "do" C
data Command
  = Asgn String SimpleExp
  | Command :+: Command
  | Skip
  | Ret SimpleExp
  | If Condition Command Command
  | While Condition Command
  deriving Eq
infixr 2 :+:

instance Show Command where
  show = show' 0
    where
      show' n (Asgn v exp) = replicate n ' ' ++ v ++ " := " ++ show exp
      show' n (c :+: c')   = show' n c ++ "\n" ++ show' n c'
      show' _ Skip         = "[DO NOTHING]"
      show' n (Ret exp)    = replicate n ' ' ++ show exp
      show' n (If b c c')  = replicate n ' ' ++ "if " ++ show b ++ "\n" ++ 
                             show' (n + 2) c ++ "\n" ++ 
                             replicate n ' ' ++ "else\n" ++ show' (n + 2) c'
      show' n (While b c)  = replicate n ' ' ++ "while " ++ show b ++ "\n" ++ 
                             show' (n + 2) c

-- | The "State" or "Context" of the expression.
type Context = Map String SimpleExp
