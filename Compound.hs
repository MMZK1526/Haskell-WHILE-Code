module Compound where

import SimpleExp

-- | Compound:
-- C ::= v = E | C; C | "skip"
data Compound
  = Asgn String SimpleExp
  | Compound :+: Compound
  | Skip
  deriving Eq

instance Show Compound where
  show (Asgn v exp) = v ++ " = " ++ show exp
  show (c :+: c')   = show c ++ ";\n" ++ show c'
  show Skip         = ";\n"
