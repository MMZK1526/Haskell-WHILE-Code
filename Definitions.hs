module Definitions where

import Data.Map ( Map )
import Utilities ( addBrace, applyOn )

-- | Simple Expression:
-- E ::= v | n | E + E | E * E
data SimpleExp
  = EVar {var :: String}
  | Nmbr { num :: Integer }
  | Plus { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | Prod { exp1 :: SimpleExp, exp2 :: SimpleExp }
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

instance Show SimpleExp where
  show (Nmbr n) = show n
  show (EVar v) = v
  show (Plus e e')
    = show e ++
      " + " ++
      applyOn (isPlus e') addBrace (show e')
  show (Prod e e')
    = applyOn (isPlus e) addBrace (show e) ++
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
  | CLT  SimpleExp SimpleExp
  | CGT  SimpleExp SimpleExp
  | CEQ  SimpleExp SimpleExp
  | CLE  SimpleExp SimpleExp
  | CNE  SimpleExp SimpleExp
  | CGE  SimpleExp SimpleExp
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
data Command
  = Asgn String SimpleExp
  | Command :+: Command
  | Skip
  | Ret SimpleExp
  | If Condition Command Command
  deriving Eq
infixr 2 :+:

instance Show Command where
  show (Asgn v exp) = v ++ " := " ++ show exp
  show (c :+: c')   = show c ++ "\n" ++ show c'
  show Skip         = "[LINE FINISHED]"
  show (Ret exp)    = show exp
  show (If b c c')  = "if " ++ show b ++ "\n  " ++ 
                      show c ++ "\nelse\n  " ++ show c'

-- | The "State" or "Context" of the expression.
type Context = Map String SimpleExp
