{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module Command where

import qualified Data.Map as M
import Control.Monad.Trans.State
import SimpleExp
import Expression
import Definitions
import Control.Monad.Trans
import EvalError
import Text.Parsec.String
import Text.Parsec
import Utilities ( eatWSP )
import Token ( parseIdentifier, parseReservedOp, parseReserved )

instance Expression Command where
  -- | Is normal (irreducible).
  {-# INLINE isNormal #-}
  isNormal :: Command -> Bool
  isNormal Skip    = True
  isNormal (Ret r) = isNormal r
  isNormal _       = False

  -- | Big-Step evaluation. Encoded with an error if cannot reach normal state.
  evalS :: Command -> StateT Context (Either EvalError) Command
  evalS lang = do
    c <- get
    case lang of
      Ret exp    -> Ret <$> evalS exp
      Skip       -> return Skip
      Asgn x exp -> do
        exp <- evalS exp
        case exp of
          EVal v -> put $ updateVarCon (M.insert x v) c
          _      -> lift $ Left TypeError
        return Skip
      c :+: c'   -> do
        r <- evalS c
        case r of
          Ret res -> return $ Ret res
          Skip    -> evalS c'
          _       -> lift $ Left TypeError
      If b c c'  -> do
        cond  <- evalS b
        case cond of
          EVal (VBool True)  -> evalS c
          EVal (VBool False) -> evalS c'
          _                  -> lift $ Left TypeError
      While b c  -> evalS $ If b (c :+: While b c) Skip

  -- | Small-Step evaluation. Encoded with an error if either in normal form or
  -- stuck state.
  eval1S :: Command -> StateT Context (Either EvalError) Command
  eval1S lang = modify' clearRules >> go lang
    where
      go lang = do
        ctxt <- gets clearRules
        case lang of
          Skip :+: com                  -> do
            modify' (applyRule E_SKIP) 
            return com
          com :+: com'                  -> do
            lang' <- go com
            case lang' of
              Ret (EVal v) -> return $ Ret $ EVal v
              com''        -> return $ com'' :+: com'
          Asgn x (EVal v)               -> do
            put $ updateVarCon (M.insert x v) ctxt
            modify' (applyRule E_ASSIGN)
            return Skip
          Asgn x exp                    -> do
            exp' <- eval1S exp
            modify' (applyRule E_ASSIGN)
            return $ Asgn x exp'
          If (EVal (VBool True)) com _  -> do
            modify' (applyRule E_IF_TRUE)
            return com
          If (EVal (VBool False)) _ com -> do
            modify' (applyRule E_IF_FALSE)
            return com
          If (EVal _) _ _               -> lift (Left TypeError)
          If b com com'                 -> do
            b' <- eval1S b
            modify' (applyRule E_IF)
            return $ If b' com com'
          Ret exp                       -> do
            modify' (applyRule E_RETURN)
            Ret <$> eval1S exp
          While b c                     -> do
            modify' (applyRule E_WHILE)
            return $ If b (c :+: While b c) Skip
          _                             -> lift (Left NormalFormError)

-- Parses a Command.
parseCom :: String -> Either ParseError Command
parseCom = parse comParser "While Command Parser: "

-- | The parser for Command.
comParser :: Parser Command
comParser = seqParser 0 <* eof
  where
    blockParser n  =
          try assignParser
      <|> whileParser n
      <|> ifParser n
      <|> returnParser
      <|> skipParser
    skipParser     = eatWSP >> return Skip
    returnParser   = do
      parseReserved "return" <|> return ()
      Ret <$> expParser'
    assignParser   = do
      v   <- parseIdentifier
      parseReservedOp ":="
      Asgn v <$> expParser'
    seqParser n    = do
      com <- blockParser n
      (eof >> return com) <|> try (char '\n' >> eof >> return com) <|> do
      try (do char '\n'
              indentParser n
              com' <- seqParser n
              return $ com :+: com'
          ) <|> return com
    indentParser n = count n (char ' ')
      <?> "indentation of " ++ show n ++ " spaces!"
    whileParser n  = do
      parseReserved "while"
      exp <- expParser'
      char '\n'
      indentParser (n + 2)
      com <- seqParser (n + 2)
      return $ While exp com
    ifParser n     = do
      parseReserved "if"
      exp  <- expParser'
      char '\n'
      indentParser (n + 2)
      com  <- seqParser (n + 2)
      char '\n'
      indentParser n
      parseReserved "else"
      char '\n'
      indentParser (n + 2)
      com' <- seqParser (n + 2)
      return $ If exp com com'
