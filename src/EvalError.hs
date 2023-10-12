module EvalError where

-- | The error type for our language.
data EvalError
  = TypeError
  | UndefVarError String
  | NormalFormError
  | ArithmeticError

instance Show EvalError where
  show TypeError         = "Type Error!"
  show (UndefVarError v) = "Undefined Variable " ++ v ++ "!"
  show NormalFormError   = "Already in Normal Form!"
  show ArithmeticError   = "Arithmetic Error!"
