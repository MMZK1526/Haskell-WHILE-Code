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
import EvalError

data DebugType
  = FullSteps
  | StepByStep
  | NoDebug
  deriving (Show, Eq)

data WhileOptions
  = OpHelp
  | OpDebug DebugType
  | OpUnknown String
  deriving (Show, Eq)

data WhileConfig
  = Config
    { isHelp :: Bool
    , err :: Maybe String
    , debugType :: DebugType
    , typeCheck :: Bool
    } deriving (Show, Eq)

{-# INLINE emptyConfig #-}
emptyConfig :: WhileConfig
emptyConfig = Config 
  { debugType = NoDebug
  , typeCheck = False 
  , isHelp = False
  , err = Nothing
  }

getConfig :: [WhileOptions] -> WhileConfig
getConfig []
  = emptyConfig
getConfig (o : os) = case o of
  OpDebug t   -> config { debugType = t }
  OpHelp      -> emptyConfig { isHelp = True }
  OpUnknown e -> emptyConfig { err = Just e }
  where
     config = getConfig os

usage :: IO ()
usage = do
  T.putStrLn "Usage: runghc main [-h] [--debug=full|step] <while_code.txt>\
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
    FullSteps  -> evalStarPrintS context command
    StepByStep -> debugWhile     context command
    NoDebug    -> case evalS' context command of
      Right Skip    -> putStrLn "Result: void"
      Right (Ret e) -> putStrLn $ "Result: " ++ show e
      Left err      -> putStrLn $ "Error evaluating " ++ src ++ ".\n"
                    ++ show err
      _             -> error "UNREACHABLE!!"

-- | Interactive debugger.
debugWhile :: Context -> Command -> IO ()
debugWhile ctxt com = introMsg >> putStrLn "" >> go ctxt com 0 True
  where 
    introMsg                 = do
      putStrLn "Press 'x' to dump the context."
      putStrLn "Press 's' to go to the next step."
      putStrLn "Press 'r' to go straight to the result."
      -- putStrLn "Press enter to go to the next line."
      putStrLn "Press 'q' to quit."
    finish ctxt com          = do
      putStrLn "Program completed!"
      putStrLn $ 
        "Result: " ++ if com == Skip then "void" else show com
      print ctxt
    go ctxt com i isPrinting = do
      let debugCycle = do
              e <- if isPrinting then getLine else return "r"
              if      e `elem` ["x", "dump"]
              then    print ctxt >> debugCycle
              else if e `elem` ["s", "step"]
              then    case runStateT (eval1S com) ctxt of
                Left NormalFormError -> finish ctxt com
                Left err             -> do
                  putStrLn "An error occurs..."
                  print err
                  print ctxt
                Right (com, ctxt) -> go ctxt com (i + 1) True
              else if e `elem` ["r", "ret", "return"]
              then    case runStateT (eval1S com) ctxt of
                Left NormalFormError -> finish ctxt com
                Left err             -> do
                  putStrLn "An error occurs..."
                  print err
                  print ctxt
                Right (com, ctxt) -> go ctxt com (i + 1) False
              else if e `elem` ["q", "quit"]
              then   return ()
              else   putStrLn "Unrecognised input!" >> introMsg >> debugCycle
      when isPrinting $ do
        putStrLn $ "Step " ++ show i ++ ":"
        print com
      debugCycle

main :: IO ()
main = do
  args <- getArgs
  -- Get all options
  let (ops, ins, errs) = getOpt
          Permute
        [ Option "h" ["help"] (NoArg OpHelp) "The Manual Page."
        , Option "d" ["debug"] (OptArg (\case
            Just "full" -> OpDebug FullSteps
            Just "step" -> OpDebug StepByStep
            Just x      -> OpUnknown x
            _           -> OpDebug StepByStep) "DEBUG") "Debug Options"
        ] args
  if not $ null errs
  then putStr (head errs) >> help
  else do
  -- Parse option arguments
  let config = getConfig ops
  if   isHelp config
  then usage
  else case err config of
    Just e -> do
      putStrLn $ "Unknown parameter " ++ e ++ "!"
      help
    _      -> 
      if      null ins
      then    help
      else if not $ null errs
      then    do
      print (head errs)
      help
      else    do
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
          Right com  -> runWhile src config (Context $ M.fromList context) com
