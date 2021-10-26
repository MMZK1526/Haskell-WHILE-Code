{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimpleExp where

import qualified Data.Map as M
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Expression ( Expression(..) )
import Definitions
import Text.Parsec hiding (State)
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Utilities ( eatBlankSpace, int )
import Control.Monad

instance Expression SimpleExp where
  -- | Is normal (irreducible).
  {-# INLINE isNormal #-}
  isNormal :: SimpleExp -> Bool
  isNormal EVal {} = True
  isNormal _       = False

  -- | Big-Step evaluation.
  evalS :: SimpleExp -> StateT Context Maybe SimpleExp
  evalS e = do
    c <- get
    case e of
      EVal v    -> return $ EVal v -- B-Val
      EVar v    ->                 -- B-Var
        EVal <$> lift (M.lookup v c)
      Plus e e' -> do              -- B-Add
        EVal l <- evalS e
        EVal r <- evalS e'
        if isNum l && isNum r
          then return $ EVal $ VNum $ fromNum l + fromNum r
          else lift Nothing
      Mnus e e' -> do              -- B-Neg
        EVal l <- evalS e
        EVal r <- evalS e'
        if isNum l && isNum r
          then return $ EVal $ VNum $ fromNum l - fromNum r
          else lift Nothing
      Prod e e' -> do              -- B-Mul
        EVal l <- evalS e
        EVal r <- evalS e'
        if isNum l && isNum r
          then return $ EVal $ VNum $ fromNum l * fromNum r
          else lift Nothing
      ELT (EVal (VNum n)) (EVal (VNum n')) -> -- B-LT
        return $ if n < n' then eTOP else eBTM
      ELT e e'           -> do
        EVal l <- evalS e
        EVal r <- evalS e'
        if isNum l && isNum r
          then return $ if fromNum l < fromNum r then eTOP else eBTM
          else lift Nothing
      EGT (EVal (VNum n)) (EVal (VNum n')) -> -- B-GT
        return $ if n > n' then eTOP else eBTM
      EGT e e'           -> do
        EVal l <- evalS e
        EVal r <- evalS e'
        if isNum l && isNum r
          then return $ if fromNum l > fromNum r then eTOP else eBTM
          else lift Nothing
      EEQ (EVal (VNum n)) (EVal (VNum n')) -> -- B-EQ
        return $ if n == n' then eTOP else eBTM
      EEQ e e'           -> do
        EVal l <- evalS e
        EVal r <- evalS e'
        if isNum l && isNum r
          then return $ if fromNum l == fromNum r then eTOP else eBTM
          else lift Nothing
      ELE (EVal (VNum n)) (EVal (VNum n')) -> -- B-LE
         return $ if n <= n' then eTOP else eBTM
      ELE e e'           -> do
        EVal l <- evalS e
        EVal r <- evalS e'
        if isNum l && isNum r
          then return $ if fromNum l <= fromNum r then eTOP else eBTM
          else lift Nothing
      ENE (EVal (VNum n)) (EVal (VNum n')) -> -- B-NE
         return $ if n /= n' then eTOP else eBTM
      ENE e e'           -> do
        EVal l <- evalS e
        EVal r <- evalS e'
        if isNum l && isNum r
          then return $ if fromNum l /= fromNum r then eTOP else eBTM
          else lift Nothing
      EGE (EVal (VNum n)) (EVal (VNum n')) -> -- B-GE
         return $ if n >= n' then eTOP else eBTM
      EGE e e'           -> do
        EVal l <- evalS e
        EVal r <- evalS e'
        if isNum l && isNum r
          then return $ if fromNum l >= fromNum r then eTOP else eBTM
          else lift Nothing

  -- | Small-Step evaluation. Encoded with Nothing if either in normal form or
  -- stuck state.
  eval1S :: SimpleExp -> StateT Context Maybe SimpleExp
  eval1S e = do
    c <- get
    case e of
      Plus e e' -> case (e, e') of -- W-EXP.ADD
        (EVal (VNum n), EVal (VNum n'))
          -> return (EVal $ VNum $ n + n')
        (EVal (VNum n), e')
          -> Plus (EVal (VNum n)) <$> eval1S e'
        (e,      e') -> do
          e <- eval1S e
          return (Plus e e')
      Mnus e e' -> case (e, e') of -- W-EXP.MINUS
        (EVal (VNum n), EVal (VNum n'))
          -> return (EVal $ VNum $ n - n')
        (EVal (VNum n), e')
          -> Mnus (EVal (VNum n)) <$> eval1S e'
        (e,      e') -> do
          e <- eval1S e
          return (Mnus e e')
      Prod e e' -> case (e, e') of -- W-EXP.MUL
        (EVal (VNum n), EVal (VNum n'))
          -> return (EVal $ VNum $ n * n')
        (EVal (VNum n), e')
          -> Prod (EVal (VNum n)) <$> eval1S e'
        (e,      e') -> do
          e <- eval1S e
          return (Prod e e')
      -- COMPARISON RULES:
      ELT (EVal (VNum n)) (EVal (VNum n'))
        -> return $ if n < n' then eTOP else eBTM
      ELT (EVal (VNum n)) e'
        -> ELT (EVal (VNum n)) <$> eval1S e'
      ELT e e'
        -> liftM2 ELT (eval1S e) (return e')
      EGT (EVal (VNum n)) (EVal (VNum n'))
        -> return $ if n > n' then eTOP else eBTM
      EGT (EVal (VNum n)) e'
        -> EGT (EVal (VNum n)) <$> eval1S e'
      EGT e e'
        -> liftM2 EGT (eval1S e) (return e')
      EEQ (EVal (VNum n)) (EVal (VNum n'))
        -> return $ if n == n' then eTOP else eBTM
      EEQ (EVal (VNum n)) e'
        -> EEQ (EVal (VNum n)) <$> eval1S e'
      EEQ e e'
        -> liftM2 EEQ (eval1S e) (return e')
      ELE (EVal (VNum n)) (EVal (VNum n'))
        -> return $ if n <= n' then eTOP else eBTM
      ELE (EVal (VNum n)) e'
        -> ELE (EVal (VNum n)) <$> eval1S e'
      ELE e e'
        -> liftM2 ELE (eval1S e) (return e')
      ENE (EVal (VNum n)) (EVal (VNum n'))
        -> return $ if n /= n' then eTOP else eBTM
      ENE (EVal (VNum n)) e'
        -> ENE (EVal (VNum n)) <$> eval1S e'
      ENE e e'
        -> liftM2 ENE (eval1S e) (return e')
      EGE (EVal (VNum n)) (EVal (VNum n'))
        -> return $ if n >= n' then eTOP else eBTM
      EGE (EVal (VNum n)) e'
        -> EGE (EVal (VNum n)) <$> eval1S e'
      EGE e e'
        -> liftM2 EGE (eval1S e) (return e')
      -- BASE RULES:
      EVar v -> do
        EVal <$> lift (M.lookup v c)
      EVal _ -> lift Nothing

-- | The parser for SimpleExp
expParser :: Parser SimpleExp
expParser = eatBlankSpace >> parser' <* eof
  where
    parser' = buildExpressionParser expTable expTerm <?> "Expression"
    TokenParser
      { parens = expParens
      , identifier = expIdentifier
      , reservedOp = expReservedOp
      , reserved = expReserved
      } = makeTokenParser $ emptyDef
          { identStart = letter
          , identLetter = alphaNum
          , caseSensitive = True
          , opStart = oneOf ""
          , opLetter = oneOf ""
          , reservedOpNames = ["+", "-", "*", "<=", ">=", "==", "!=", "<", ">"]
          , reservedNames = ["true", "false"]
          }
    expTerm =
      expParens parser' <|>
      EVar <$> expIdentifier <|>
      EVal . VNum <$> int <|>
      EVal <$> (expReserved "true" >> return (VBool True)) <|>
      EVal <$> (expReserved "false" >> return (VBool False))
    expTable =
      [ [ Infix (expReservedOp "*" >> return Prod) AssocLeft ]
      , [ Infix (expReservedOp "+" >> return Plus) AssocLeft
        , Infix (expReservedOp "-" >> return Mnus) AssocLeft
        ]
      , [ Infix (expReservedOp "<=" >> return ELE) AssocLeft
        , Infix (expReservedOp ">=" >> return EGE) AssocLeft
        , Infix (expReservedOp "<" >> return ELT) AssocLeft
        , Infix (expReservedOp ">" >> return EGT) AssocLeft
        ]
      , [ Infix (expReservedOp "!=" >> return ENE) AssocLeft
        , Infix (expReservedOp "=" >> return EEQ) AssocLeft
        ]
      ]
