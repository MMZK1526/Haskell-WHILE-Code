{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Token where

import Text.Parsec
import Text.Parsec.Token
parseParens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parseIdentifier :: Stream s m Char => ParsecT s u m String
parseReservedOp :: Stream s m Char => String -> ParsecT s u m ()
parseReserved :: Stream s m Char => String -> ParsecT s u m ()

TokenParser
  { parens = parseParens
  , identifier = parseIdentifier
  , reservedOp = parseReservedOp
  , reserved = parseReserved
  } = makeTokenParser $ LanguageDef
    { identStart = letter
    , identLetter = alphaNum
    , caseSensitive = True
    , opStart = oneOf ""
    , opLetter = oneOf ""
    , reservedOpNames
    = [ "+", "-", "*", "<=", ">=", "==", "!=", "<", ">", "&", "|", "!", ":=,"
      , "&&", "||", "/", "%"
      ]
    , reservedNames = ["true", "false", "if", "else", "while"]
    , commentStart = ""
    , commentEnd = ""
    , commentLine = ""
    , nestedComments = False
    , usedSpaces = (== ' ')
    }
