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

{-# INLINE fromNmbr #-}
fromNmbr :: SimpleExp -> Integer
fromNmbr (Nmbr n) = n
fromNmbr _        = error "Cannot extract from a non-value!"

instance Expression SimpleExp where
  -- | Is normal (irreducible).
  {-# INLINE isNormal #-}
  isNormal :: SimpleExp -> Bool
  isNormal = isNmbr

  -- | Big-Step evaluation.
  evalS :: SimpleExp -> State Context SimpleExp
  evalS exp = do
    c <- get
    case exp of
      Nmbr n    -> return $ Nmbr n -- B-Num
      Plus e e' -> do              -- B-Add
        l <- evalS e
        r <- evalS e'
        if isNmbr l && isNmbr r
          then return $ Nmbr $ fromNmbr l + fromNmbr r
          else return $ l + r
      Mnus e e' -> do              -- B-Neg
        l <- evalS e
        r <- evalS e'
        if isNmbr l && isNmbr r
          then return $ Nmbr $ fromNmbr l - fromNmbr r
          else return $ l - r
      Prod e e' -> do              -- B-Mul
        l <- evalS e
        r <- evalS e'
        if isNmbr l && isNmbr r
          then return $ Nmbr $ fromNmbr l * fromNmbr r
          else return $ l * r
      EVar v    -> do
        let mv = M.lookup v c
        case mv of
          Nothing -> return $ EVar v
          Just e  -> return e

  -- | Small-Step evaluation. Encoded with Nothing if either in normal form or
  -- stuck state.
  eval1S :: SimpleExp -> StateT Context Maybe SimpleExp
  eval1S exp = do
    c <- get
    case exp of
      Plus e e' -> case (e, e') of
        (Nmbr n, Nmbr n') -> return (Nmbr $ n + n') -- W-EXP.ADD
        (Nmbr n, e')      ->                        -- W-EXP.RIGHT
          Plus (Nmbr n) <$> eval1S e'
        (e,      e')      -> do                     -- W-EXP.LEFT
          e <- eval1S e
          return (Plus e e')
      Mnus e e' -> case (e, e') of
        (Nmbr n, Nmbr n') -> return (Nmbr $ n - n') -- W-EXP.ADD
        (Nmbr n, e')      ->                        -- W-EXP.RIGHT
          Mnus (Nmbr n) <$> eval1S e'
        (e,      e')      -> do                     -- W-EXP.LEFT
          e <- eval1S e
          return (Mnus e e')
      Prod e e' -> case (e, e') of
        (Nmbr n, Nmbr n') -> return (Nmbr $ n * n') -- W-EXP.MUL
        (Nmbr n, e')      ->                        -- W-EXP.RIGHT
          Prod (Nmbr n) <$> eval1S e'
        (e,      e')      -> do                     -- W-EXP.LEFT
          e <- eval1S e
          return (Prod e e')
      EVar v    -> do
        let e' = M.lookup v c
        maybe (lift Nothing) return e'
      Nmbr n    -> lift Nothing

-- | The parser for SimpleExp
expParser :: Parser SimpleExp
expParser = eatBlankSpace >> parser' <* eof 
  where
    parser' = buildExpressionParser expTable expTerm <?> "Expression"
    TokenParser 
      { parens = expParens
      , identifier = expIdentifier
      , reservedOp = expReservedOp
      } = makeTokenParser $ emptyDef
          { identStart = letter
          , identLetter = alphaNum
          , caseSensitive = True
          , opStart = oneOf ""
          , opLetter = oneOf ""
          , reservedOpNames = ["+", "-", "*"]
          }
    expTerm =
      expParens parser' <|>
      EVar <$> expIdentifier <|>
      Nmbr <$> int
    expTable = 
      [ [ Infix (expReservedOp "*" >> return Prod) AssocLeft ]
      , [ Infix (expReservedOp "+" >> return Plus) AssocLeft
        , Infix (expReservedOp "-" >> return Mnus) AssocLeft
        ]
      ]
