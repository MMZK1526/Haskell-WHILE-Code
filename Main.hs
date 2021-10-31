{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as M
import System.Console.GetOpt
import System.Environment
import Control.Monad
import Utilities ( eatWSP )
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
import Control.Monad.Trans.State

data DebugType
  = NoContext
  | FullContext
  | NoDebug
  deriving (Show, Eq)

data WhileOptions
  = OpHelp
  | OpDebug DebugType
  | OpUnknown String
  deriving (Show, Eq)

data WhileConfig
  = Config
    { debugType :: DebugType
    , typeCheck :: Bool
    }
  | HelpConfig
  | UnknownConfig String
  deriving (Show, Eq)

{-# INLINE emptyConfig #-}
emptyConfig :: WhileConfig
emptyConfig = Config { debugType = NoDebug, typeCheck = False }

getConfig :: [WhileOptions] -> WhileConfig
getConfig []
  = emptyConfig
getConfig (o : os) = case o of
  OpDebug t   -> config { debugType = t }
  OpHelp      -> HelpConfig
  OpUnknown e -> UnknownConfig e
  where
     config = getConfig os

usage :: IO ()
usage = do
  T.putStrLn "Usage: runghc main <while_code.txt>\
            \[<argument_name>=<value>] [...]"
  T.putStrLn "For full support, see\n\
            \https://github.com/sorrowfulT-Rex/50003-Models-of-Computation."

help :: IO ()
help = putStrLn "Run \"runghc main -h\" for help."

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
        Right (EVal e) -> return (v, e)
        Left err       -> fail $ "Failed to evaluate " ++ v ++ "! " ++ show err
        _              -> error "UNREACHABLE!!"

-- | Run the While program by the given file name, configuration, context and
-- code. The file name is used on error handling only.
runWhile :: String -> WhileConfig -> Context -> Command -> IO ()
runWhile src 
         Config { debugType = d, typeCheck = t } 
         context 
         command = do
  case d of
    NoContext   -> evalStarPrintS     context command
    FullContext -> evalStarPrintFullS context command
    NoDebug     -> case evalS' context command of
      Right Skip    -> putStrLn "Result: void"
      Right (Ret e) -> putStrLn $ "Result:" ++ show e
      Left err      -> putStrLn $ "Error evaluating " ++ src ++ ".\n"
                    ++ show err
      _             -> error "UNREACHABLE!!"
runWhile _ _ _ _ = error "UNREACHABLE!!"

main :: IO ()
main = do
  args <- getArgs
  -- Get all options
  let (ops, ins, errs) = getOpt
          Permute
        [ Option "h" ["help"] (NoArg OpHelp) "The Manual Page."
        , Option "d" ["debug"] (OptArg (\case
            Just "full" -> OpDebug FullContext
            Just x      -> OpUnknown x
            _           -> OpDebug NoContext) "DEBUG") "Debug Options"
        ] args
  if not $ null errs
  then putStr (head errs) >> help
  else do
  -- Parse option arguments
  let config = getConfig ops
  case config of
    HelpConfig      -> usage
    UnknownConfig e -> do
      putStrLn $ "Unknown parameter " ++ e ++ "!"
      help
    _               -> if null ins
      then help
      else if not $ null errs
      then do
      print (head errs)
      help
      else do
      -- Parse source code and arguments
      let (src : args) = ins
      case forM args parseArg of
        Left error    -> print error
        Right context -> do
        handleDNE ((>> help) . print) $ do
        text <- T.readFile src
        case parseCom $ T.unpack text of
          Left error -> print error
          -- Run the program
          Right com  -> runWhile src config (M.fromList context) com
