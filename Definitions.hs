module Definitions where

import Data.Map ( Map )
import Utilities ( addBrace, applyOn )

-- | For simplicity, I have combined the conditionals into expression.
-- Simple Expression:
-- E ::= v | n | E + E | E - E | E * E | "true" | "false" | "not" E | E "and" E
-- | E "or" E  | E < E | E = E | E > E | E <= E | E != E | E >= E
data SimpleExp
  = EVar {var :: String}
  | EVal {val :: Value} -- Number or Boolean
  | Plus { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | Mnus { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | Prod { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | ELT  { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | EGT  { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | EEQ  { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | ELE  { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | EGE  { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | ENE  { exp1 :: SimpleExp, exp2 :: SimpleExp }
  deriving (Eq)

{-# INLINE getPrecedence #-}
getPrecedence :: SimpleExp -> Int
getPrecedence Prod {} = 12
getPrecedence Plus {} = 11
getPrecedence Mnus {} = 11
getPrecedence ELT {} = 9
getPrecedence EGT {} = 9
getPrecedence ELE {} = 9
getPrecedence EGE {} = 9
getPrecedence ENE {} = 8
getPrecedence EEQ {} = 8
getPrecedence _      = 114514

{-# INLINE precedenceOrdering #-}
precedenceOrdering :: SimpleExp -> SimpleExp -> Ordering
precedenceOrdering exp1 exp2 = compare (getPrecedence exp1) (getPrecedence exp2)

instance Show SimpleExp where
  show (EVal v) = show v
  show (EVar v) = v
  show exp@(Plus e e')
    = applyOn (precedenceOrdering exp e' == GT) addBrace (show e) ++
      " + " ++
      applyOn (precedenceOrdering exp e' `elem` [GT, EQ]) addBrace (show e')
  show exp@(Mnus e e')
    = applyOn (precedenceOrdering exp e == GT) addBrace (show e) ++
      " - " ++
      applyOn (precedenceOrdering exp e' `elem` [GT, EQ]) addBrace (show e')
  show exp@(Prod e e')
    = applyOn (precedenceOrdering exp e == GT) addBrace (show e) ++
      " * " ++
      applyOn (precedenceOrdering exp e' `elem` [GT, EQ]) addBrace (show e')
  show exp@(ELT e e')
    = applyOn (precedenceOrdering exp e == GT) addBrace (show e) ++
      " < " ++
      applyOn (precedenceOrdering exp e' `elem` [GT, EQ]) addBrace (show e')
  show exp@(EGT e e')
    = applyOn (precedenceOrdering exp e == GT) addBrace (show e) ++
      " > " ++
      applyOn (precedenceOrdering exp e' `elem` [GT, EQ]) addBrace (show e')
  show exp@(ENE e e')
    = applyOn (precedenceOrdering exp e == GT) addBrace (show e) ++
      " != " ++
      applyOn (precedenceOrdering exp e' `elem` [GT, EQ]) addBrace (show e')
  show exp@(EEQ e e')
    = applyOn (precedenceOrdering exp e == GT) addBrace (show e) ++
      " = " ++
      applyOn (precedenceOrdering exp e' `elem` [GT, EQ]) addBrace (show e')
  show exp@(ELE e e')
    = applyOn (precedenceOrdering exp e == GT) addBrace (show e) ++
      " <= " ++
      applyOn (precedenceOrdering exp e' `elem` [GT, EQ]) addBrace (show e')
  show exp@(EGE e e')
    = applyOn (precedenceOrdering exp e == GT) addBrace (show e) ++
      " >= " ++
      applyOn (precedenceOrdering exp e' `elem` [GT, EQ]) addBrace (show e')

instance Num SimpleExp where
  x + y       = Plus x y
  x * y       = Prod x y
  fromInteger = EVal . VNum
  abs         = undefined
  signum      = undefined
  negate      = undefined

-- | This is our entire language.
-- Note here we have 2 kinds of answer configuration, namely "skip" and 
-- "return" E (if E is in normal form as well).
-- This is different to what's introduced in the course (where there is no
-- "return"), but with "return" we can see the evaluated result without looking
-- into the state.
-- Command:
-- C ::= v = E | C; C | "skip" | "return" E | "if" E "then" C "else" C
-- | "while" E "do" C
data Command
  = Asgn String SimpleExp
  | Command :+: Command
  | Skip
  | Ret SimpleExp
  | If SimpleExp Command Command
  | While SimpleExp Command
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
type Context = Map String Value

-- | The "result" type that encodes the type information of the value.
data Value = VNum Integer | VBool Bool
  deriving Eq

instance Show Value where
  show (VNum n)      = show n
  show (VBool True)  = "true"
  show (VBool False)  = "false"

{-# INLINE isNum #-}
{-# INLINE isBool #-}
isNum, isBool :: Value -> Bool
isNum VNum {}   = True
isNum _         = False
isBool VBool {} = True
isBool _        = False

{-# INLINE getType #-}
getType :: Value -> String
getType VNum {}  = "Integer"
getType VBool {} = "Bool"

{-# INLINE fromNum #-}
fromNum :: Value -> Integer
fromNum (VNum n) = n
fromNum _        = error "Extract from invalid number!"

{-# INLINE fromBool #-}
fromBool :: Value -> Bool
fromBool (VBool b) = b
fromBool _         = error "Extract from invalid boolean!"

{-# INLINE eTOP #-}
{-# INLINE eBTM #-}
eTOP, eBTM :: SimpleExp
eTOP = EVal $ VBool True
eBTM = EVal $ VBool False

instance Num Value where
  x + y       = VNum $ fromNum x + fromNum y
  x * y       = VNum $ fromNum x * fromNum y
  fromInteger = VNum
  abs         = VNum . abs . fromNum
  signum      = VNum . signum . fromNum
  negate      = VNum . negate . fromNum
