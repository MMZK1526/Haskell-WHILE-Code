{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Control.Applicative( (<*) )
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Definitions
import Utilities ( eatBlankSpace, int )

-- mainparser :: Parser Stmt
-- mainparser = m_whiteSpace >> stmtparser <* eof
--     where
--       stmtparser :: Parser Stmt
--       stmtparser = fmap Seq (m_semiSep1 stmt1)
--       stmt1 = (m_reserved "nop" >> return Nop)
--               <|> do { v <- m_identifier
--                      ; m_reservedOp ":="
--                      ; e <- exprparser
--                      ; return (v := e)
--                      }
--               <|> do { m_reserved "if"
--                      ; b <- exprparser
--                      ; m_reserved "then"
--                      ; p <- stmtparser
--                      ; m_reserved "else"
--                      ; q <- stmtparser
--                      ; m_reserved "fi"
--                      ; return (If b p q)
--                      }
--               <|> do { m_reserved "while"
--                      ; b <- exprparser
--                      ; m_reserved "do"
--                      ; p <- stmtparser
--                      ; m_reserved "od"
--                      ; return (While b p)
--                      }
