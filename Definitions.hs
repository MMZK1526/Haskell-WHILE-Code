module Definitions where

import Data.Map ( Map )
import Utilities ( addBrace, applyOn )
import Data.Maybe
import qualified Data.Map as M

-- | For simplicity, I have combined the conditionals into expression.
-- Simple Expression:
-- E ::= v | n | E + E | E - E | E * E | "true" | "false" | "not" E | E "and" E
-- | E "or" E  | E < E | E = E | E > E | E <= E | E != E | E >= E | E / E
-- | E % E
data SimpleExp
  = EVar { var  :: String }
  | EVal { val  :: Value } -- Number or Boolean
  | Plus { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | Mnus { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | Prod { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | Div  { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | Mod  { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | ELT  { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | EGT  { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | EEQ  { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | ELE  { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | EGE  { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | ENE  { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | And  { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | Or   { exp1 :: SimpleExp, exp2 :: SimpleExp }
  | Not  { exp1 :: SimpleExp }
  deriving (Eq)

{-# INLINE getPrec #-}
getPrec :: SimpleExp -> Int
getPrec Not  {} = 13
getPrec Prod {} = 12
getPrec Plus {} = 11
getPrec Mnus {} = 11
getPrec Div  {} = 11
getPrec Mod  {} = 11
getPrec ELT  {} = 9
getPrec EGT  {} = 9
getPrec ELE  {} = 9
getPrec EGE  {} = 9
getPrec ENE  {} = 8
getPrec EEQ  {} = 8
getPrec And  {} = 4
getPrec Or   {} = 3
getPrec _       = 114514

{-# INLINE getOpSymbol #-}
getOpSymbol :: SimpleExp -> String
getOpSymbol Prod {} = " * "
getOpSymbol Plus {} = " + "
getOpSymbol Mnus {} = " - "
getOpSymbol Div  {} = " / "
getOpSymbol Mod  {} = " % "
getOpSymbol ELT  {} = " < "
getOpSymbol EGT  {} = " > "
getOpSymbol ELE  {} = " <= "
getOpSymbol EGE  {} = " >= "
getOpSymbol ENE  {} = " != "
getOpSymbol EEQ  {} = " = "
getOpSymbol And  {} = " & "
getOpSymbol Or   {} = " | "
getOpSymbol Not  {} = " ! "
getOpSymbol _       = undefined

{-# INLINE precOrd #-}
precOrd :: SimpleExp -> SimpleExp -> Ordering
precOrd exp1 exp2
  = compare (getPrec exp1) (getPrec exp2)

instance Show SimpleExp where
  show (EVal v) = show v
  show (EVar v) = v
  show exp@(Not b)
    = '!' : applyOn (precOrd exp e /= LT) addBrace (show e)
    where
      e = exp1 exp
  show exp
    = applyOn (precOrd exp e == GT) addBrace (show e) ++
      getOpSymbol exp ++
      applyOn (precOrd exp e' /= LT) addBrace (show e')
    where
      e  = exp1 exp
      e' = exp2 exp

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
  | RetVoid
  | If SimpleExp Command Command
  | While SimpleExp Command
  deriving Eq
infixr 2 :+:

instance Show Command where
  show = show' 0
    where
      show' n (Asgn v exp)  = replicate n ' ' ++ v ++ " := " ++ show exp
      show' n (c :+: c')    = show' n c ++ "\n" ++ show' n c'
      show' n Skip          = replicate n ' ' ++ "[DO NOTHING]"
      show' n (Ret exp)     = replicate n ' ' ++ "return " ++ show exp
      show' n c@If {}       = showIf n c
      show' n (While b c)   = replicate n ' ' ++ "while " ++ show b ++ "\n" ++
                              show' (n + 2) c
      show' n RetVoid       = replicate n ' ' ++ "return"

      showIf n (If b c c')
        = replicate n ' ' ++ "if " ++ show b ++ "\n" ++ showIfBody (n + 2) c c'
      showIf _ _ 
        = undefined

      showIfBody n c Skip          = show' n c
      showIfBody n c (If b c' c'') = show' n c ++ "\n" ++ 
                                     replicate (n - 2) ' ' ++ "elif " ++ 
                                     show b ++ "\n" ++ showIfBody n c' c''
      showIfBody n c c'            = show' n c ++ "\n" ++ 
                                     replicate (n - 2) ' ' ++ "else\n" ++ 
                                     show' n c'

-- | The "State" or "Context" of the expression.
data Context = Context
  { varCon :: Map String Value -- ^ The variable context
  , rules :: [Rule]  -- ^ The rules in this step; not used for big-step
  }

-- | The evaluating rules.
data Rule
  = E_VAR
  | E_ADD
  | E_SUB
  | E_MULT
  | E_LT
  | E_GT
  | E_LE
  | E_GE
  | E_EQ
  | E_NE
  | E_AND_TRUE
  | E_AND_FALSE
  | E_AND_EXP
  | E_OR_TRUE
  | E_OR_FALSE
  | E_OR_EXP
  | E_SKIP
  | E_ASSIGN_EXP
  | E_ASSIGN_VAL
  | E_WHILE
  | E_IF_EXP
  | E_IF_TRUE
  | E_IF_FALSE
  | E_RETURN
  | E_NOT
  | E_DIV
  | E_MOD
  deriving (Eq, Show)

-- | The empty context.
emptyContext :: Context
emptyContext = Context M.empty []

-- | Update the variable state of Context.
updateVarCon :: (Map String Value -> Map String Value) -> Context -> Context
updateVarCon f (Context vc rs) = Context (f vc) rs

-- | Remove rule records from Context.
clearRules :: Context -> Context
clearRules (Context vc _) = Context vc []

-- | Add a rule record to Context.
applyRule :: Rule -> Context -> Context
applyRule r (Context vc rs) = Context vc $ r : rs

-- | Print everything from the Context.
dumpContext :: Context -> String
dumpContext ctxt = show ctxt ++ '\n' : ("Rules applied :" ++ show (rules ctxt))

instance Show Context where
  show = ("Context: " ++). show . M.toList . varCon

-- | The "result" type that encodes the type information of the value.
data Value = VNum Integer | VBool Bool
  deriving Eq

instance Show Value where
  show (VNum n)      = show n
  show (VBool True)  = "true"
  show (VBool False) = "false"

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

{-# INLINE fromNumMaybe #-}
fromNumMaybe :: Value -> Maybe Integer
fromNumMaybe (VNum n) = Just n
fromNumMaybe _        = Nothing

{-# INLINE fromBoolMaybe #-}
fromBoolMaybe :: Value -> Maybe Bool
fromBoolMaybe (VBool b) = Just b
fromBoolMaybe _         = Nothing

{-# INLINE fromNum #-}
fromNum :: Value -> Integer
fromNum = fromJust . fromNumMaybe

{-# INLINE fromBool #-}
fromBool :: Value -> Bool
fromBool = fromJust . fromBoolMaybe

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
