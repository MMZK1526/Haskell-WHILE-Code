{-# LANGUAGE InstanceSigs #-}

module SimpleExp where

import qualified Data.Map as M
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Monad.Trans.State
import Expression
import Definitions
import Text.Parsec hiding (State)
import Text.Parsec.String
import Text.Parsec.Expr
import Utilities ( int, encodeErr, eatWSP )
import Control.Monad
import Data.Maybe
import EvalError
import Token

instance Expression SimpleExp where
  -- | Is normal (irreducible).
  {-# INLINE isNormal #-}
  isNormal :: SimpleExp -> Bool
  isNormal EVal {} = True
  isNormal _       = False

  -- | Big-Step evaluation. Encoded with an error if cannot reach normal state.
  evalS :: SimpleExp -> StateT Context (Either EvalError) SimpleExp
  evalS e = do
    c <- get 
    let binOp op e e' fromValMaybe toVal = do
          l <- evalS e  >>= lift . encodeErr TypeError . fromValMaybe . val
          r <- evalS e' >>= lift . encodeErr TypeError . fromValMaybe . val
          return $ EVal $ toVal $ l `op` r
    let unOp op e fromValMaybe toVal
          = EVal . toVal . op <$> 
            (evalS e >>= lift . encodeErr TypeError .fromValMaybe . val)
    case e of
      EVal v    -> return $ EVal v
      EVar v    -> EVal <$> lift 
        (encodeErr (UndefVarError v) $ M.lookup v $ varCon c)
      Plus e e' -> binOp (+)  e e' fromNumMaybe  VNum
      Mnus e e' -> binOp (-)  e e' fromNumMaybe  VNum
      Prod e e' -> binOp (*)  e e' fromNumMaybe  VNum
      Div  e e' -> binOp div  e e' fromNumMaybe  VNum
      Mod  e e' -> binOp mod  e e' fromNumMaybe  VNum
      ELT  e e' -> binOp (<)  e e' fromNumMaybe  VBool
      EGT  e e' -> binOp (>)  e e' fromNumMaybe  VBool
      ELE  e e' -> binOp (<=) e e' fromNumMaybe  VBool
      EGE  e e' -> binOp (>=) e e' fromNumMaybe  VBool
      Not  e    -> unOp  not  e    fromBoolMaybe VBool
      EEQ  e e' -> do
        let numArgs  = evalStateT (binOp (==) e e' fromNumMaybe VBool) c
        let boolArgs = evalStateT (binOp (==) e e' fromBoolMaybe VBool) c
        lift $ numArgs <> boolArgs
      ENE  e e' -> do
        let numArgs  = evalStateT (binOp (==) e e' fromNumMaybe VBool) c
        let boolArgs = evalStateT (binOp (==) e e' fromBoolMaybe VBool) c
        lift $ numArgs <> boolArgs
      And  e e' -> do
        l <- evalS e >>= lift . encodeErr TypeError . fromBoolMaybe . val
        if not l
          then return $ EVal $ VBool False
          else EVal . VBool <$> 
            (evalS e' >>= lift . encodeErr TypeError .  fromBoolMaybe . val)
      Or   e e' -> do
        l <- evalS e >>= lift . encodeErr TypeError . fromBoolMaybe . val
        if l
          then return $ EVal $ VBool True
          else EVal . VBool <$> 
            (evalS e' >>= lift . encodeErr TypeError . fromBoolMaybe . val)

  -- | Small-Step evaluation. Encoded with an error if either in normal form or
  -- stuck state.
  eval1S :: SimpleExp -> StateT Context (Either EvalError) SimpleExp
  eval1S e = modify' clearRules >> go e
    where
      go e = do
        let binOp op e e' fromValMaybe toVal toExp rule = do
              case (e, e') of
                (EVal l, EVal r) -> do
                  modify' (applyRule rule) 
                  lv <- lift $ encodeErr TypeError $ fromValMaybe l
                  rv <- lift $ encodeErr TypeError $ fromValMaybe r
                  return $ EVal $ toVal $ lv `op` rv
                (EVal l, e')     -> do
                  e' <- go e'
                  modify' (applyRule rule) 
                  return $ toExp e e'
                (e,      e')     -> do
                  e <- go e
                  modify' (applyRule rule) 
                  return $ toExp e e'
        let unOp op e fromValMaybe toVal toExp rule = case e of
              EVal v -> do
                modify' (applyRule rule) 
                EVal . toVal . op
                  <$> lift (encodeErr TypeError $ fromValMaybe v)
              e      -> do
                e <- go e
                modify' (applyRule rule) 
                return $ toExp e
        c <- get
        case e of
          EVar v    -> do
            modify' $ applyRule E_VAR
            fmap EVal $ lift $ 
              encodeErr (UndefVarError v) $ M.lookup v $ varCon c
          EVal _    -> lift $ Left NormalFormError
          Plus e e' -> binOp (+)  e e' fromNumMaybe  VNum  Plus E_ADD
          Mnus e e' -> binOp (-)  e e' fromNumMaybe  VNum  Mnus E_SUB 
          Prod e e' -> binOp (*)  e e' fromNumMaybe  VNum  Prod E_MULT
          Div  e e' -> binOp div  e e' fromNumMaybe  VNum  Div  E_DIV
          Mod  e e' -> binOp mod  e e' fromNumMaybe  VNum  Mod  E_MOD
          ELT  e e' -> binOp (<)  e e' fromNumMaybe  VBool ELT  E_LT
          EGT  e e' -> binOp (>)  e e' fromNumMaybe  VBool EGT  E_GT
          ELE  e e' -> binOp (<=) e e' fromNumMaybe  VBool ELE  E_LE
          EGE  e e' -> binOp (>=) e e' fromNumMaybe  VBool EGE  E_GE
          Not  e    -> unOp  not  e    fromBoolMaybe VBool Not  E_NOT
          ENE  e e' -> do
            let numArgs  = evalStateT 
                  (binOp (/=) e e' fromNumMaybe VBool ENE E_NE) c
            let boolArgs = evalStateT 
                  (binOp (/=) e e' fromBoolMaybe VBool ENE E_NE) c
            lift $ numArgs <> boolArgs
          EEQ e e'  -> do
            let numArgs  = 
                  evalStateT (binOp (==) e e' fromNumMaybe VBool EEQ E_EQ) c
            let boolArgs = 
                  evalStateT (binOp (==) e e' fromBoolMaybe VBool EEQ E_EQ) c
            lift $ numArgs <> boolArgs
          And e e'  -> case (e, e') of
              (EVal l, EVal r) -> do
                lv <- lift (encodeErr TypeError $ fromBoolMaybe l)
                if not lv
                  then return $ EVal $ VBool False
                  else EVal . VBool <$> lift 
                    (encodeErr TypeError $ fromBoolMaybe r)
              (EVal l, e')     -> And e <$> go e'
              (e,      e')     -> flip And e' <$> go e
          Or  e e'  -> case (e, e') of
              (EVal l, EVal r) -> do
                lv <- lift (encodeErr TypeError $ fromBoolMaybe l)
                if lv
                  then return $ EVal $ VBool True
                  else EVal . VBool <$> lift 
                    (encodeErr TypeError $ fromBoolMaybe r)
              (EVal l, e')     -> Or e <$> go e'
              (e,      e')     -> flip Or e' <$> go e

-- Parses a SimpleExp.
parseExp :: String -> Either ParseError SimpleExp
parseExp = parse expParser "Simple Expression Parser: "

-- The parser for SimpleExp (ignoring indentation).
expParser :: Parser SimpleExp
expParser = eatWSP >> expParser' <* eof

-- | The parser for SimpleExp (no indentation allowed, ingore unparseable final 
-- parts).
expParser' :: Parser SimpleExp
expParser' = buildExpressionParser expTable expTerm
 where
    expTerm
        = parseParens expParser'
      <|> EVar <$> parseIdentifier
      <|> EVal . VNum <$> int
      <|> EVal <$> (parseReserved "true" >> return (VBool True))
      <|> EVal <$> (parseReserved "false" >> return (VBool False))
    expTable =
      [ [ Prefix (parseReservedOp "!" >> return Not) ]
      , [ Infix (parseReservedOp "*" >> return Prod) AssocLeft 
        , Infix (parseReservedOp "/" >> return Div ) AssocLeft
        , Infix (parseReservedOp "%" >> return Mod ) AssocLeft
        ]
      , [ Infix (parseReservedOp "+" >> return Plus) AssocLeft
        , Infix (parseReservedOp "-" >> return Mnus) AssocLeft
        ]
      , [ Infix (parseReservedOp "<=" >> return ELE) AssocLeft
        , Infix (parseReservedOp ">=" >> return EGE) AssocLeft
        , Infix (parseReservedOp "<"  >> return ELT) AssocLeft
        , Infix (parseReservedOp ">"  >> return EGT) AssocLeft
        ]
      , [ Infix (parseReservedOp "!=" >> return ENE) AssocLeft
        , Infix (parseReservedOp "==" >> return EEQ) AssocLeft
        , Infix (parseReservedOp "="  >> return EEQ) AssocLeft
        ]
      , [ Infix  (parseReservedOp "&&" >> return And) AssocLeft
        , Infix  (parseReservedOp "&"  >> return And) AssocLeft
        ]
      , [ Infix (parseReservedOp "||" >> return Or) AssocLeft
        , Infix (parseReservedOp "|"  >> return Or) AssocLeft ]
      ]
