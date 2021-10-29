{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as M
import System.Console.GetOpt
import System.Environment
import Control.Monad
import Utilities ( forMBreak, eatWSP )
import Gadgets.IO ( handleDNE )
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Command
import Text.Parsec
import Expression
import Definitions
import Token
import SimpleExp

data WhileOptions 
  = OpHelp
  deriving Show

usage :: IO ()
usage = do 
  T.putStrLn "Usage: runghc main <while_code.txt>\
            \[<argument_name>=<value>] [...]"
  T.putStrLn "For full support, see\n\
            \https://github.com/sorrowfulT-Rex/50003-Models-of-Computation."

help :: IO ()
help = do 
  putStrLn "Run \"runghc main -h\" to for help."

-- | Parses an argument given to the While code.
parseArg :: String -> Either ParseError (String, Value)
parseArg = parse parser' "Argument Parser: "
  where
    parser' = do
      eatWSP
      v <- parseIdentifier
      parseReservedOp "=" <|> parseReservedOp "!="
      e <- eval <$> expParser
      case e of
        Just (EVal e) -> return (v, e)
        _             -> fail $ "evaluation for argument " ++ v ++ "has failed!"

main :: IO ()
main = do
  args <- getArgs
  let (ops, ins, errs) = getOpt 
          Permute 
        [ Option "h" ["help"] (NoArg OpHelp) "The manual page." 
        ] args
  (b, ops) <- forMBreak ops $ \case 
    OpHelp -> usage >> return (False, Nothing :: Maybe WhileOptions)
  when b $ if null ins 
    then help
    else do
      let (src : args) = ins
      case forM args parseArg of
        Left error    -> print error
        Right context -> do
        handleDNE ((>> help) . print) $ do
          text <- T.readFile src
          case parseCom $ T.unpack text of
            Left error -> print error
            Right com  -> do
              putStr "result: "
              case evalS' (M.fromList context) com of
                Just Skip ->    putStrLn "void"
                Just (Ret e) -> print e
                _         ->    putStrLn "Evaluation Error!"
