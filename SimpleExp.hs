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
    let binOp op e e' fromValMaybe toVal = do
          l <- evalS e >>= lift . fromValMaybe . fromEVal
          r <- evalS e' >>= lift . fromValMaybe . fromEVal
          return $ EVal $ toVal $ l `op` r
    let unOp op e fromValMaybe toVal = do
          v <- evalS e >>= lift . fromValMaybe . fromEVal
          return $ EVal $ toVal $ op v
    case e of
      EVal v    -> return $ EVal v
      EVar v    -> EVal <$> lift (M.lookup v c)
      Plus e e' -> binOp (+)  e e' fromNumMaybe VNum
      Mnus e e' -> binOp (-)  e e' fromNumMaybe VNum
      Prod e e' -> binOp (*)  e e' fromNumMaybe VNum
      ELT e e'  -> binOp (<)  e e' fromNumMaybe VBool
      EGT e e'  -> binOp (>)  e e' fromNumMaybe VBool
      ELE e e'  -> binOp (<=) e e' fromNumMaybe VBool
      EGE e e'  -> binOp (>=) e e' fromNumMaybe VBool
      EEQ e e'  -> do
        let numArgs  = evalStateT (binOp (==) e e' fromNumMaybe VBool) c
        let boolArgs = evalStateT (binOp (==) e e' fromBoolMaybe VBool) c
        lift $ numArgs `mplus` boolArgs
      ENE e e'  -> do
        let numArgs  = evalStateT (binOp (==) e e' fromNumMaybe VBool) c
        let boolArgs = evalStateT (binOp (==) e e' fromBoolMaybe VBool) c
        lift $ numArgs `mplus` boolArgs
      And e e'  -> binOp (&&) e e' fromBoolMaybe VBool
      Or  e e'  -> binOp (&&) e e' fromBoolMaybe VBool
      Not e     -> unOp  not  e    fromBoolMaybe VBool

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
      EEQ (EVal (VBool b)) (EVal (VBool b'))
        -> return $ if b == b' then eTOP else eBTM
      EEQ (EVal v) e'
        -> EEQ (EVal v) <$> eval1S e'
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
      ENE (EVal (VBool b)) (EVal (VBool b'))
        -> return $ if b /= b' then eTOP else eBTM
      ENE (EVal v) e'
        -> ENE (EVal v) <$> eval1S e'
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
      _ -> undefined

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
