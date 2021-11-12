{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Command
import Control.Monad
import Control.Monad.Trans.State
import Definitions
import EvalError
import Expression
import Gadgets.IO
import SimpleExp
import System.Console.GetOpt
import System.Environment
import Text.Parsec
import Token
import Utilities

-- | The debug modes.
data DebugMode
  = FullSteps -- ^ Print out all small-steps at once.
  | StepByStep  -- ^ Print out one step at a time, interact with user.
  | NoDebug  -- ^ No debug; show result directly with big-step (default).
  deriving (Show, Eq)

-- | The options for the While interpreter CLI.
data WhileOptions
  = OpHelp -- ^ Show help information.
  | OpDebug DebugMode -- ^ Debug mode.
  | OpUnknown String  -- ^ Unrecognised options.
  deriving (Show, Eq)

-- | The configurations for the While interpreter CLI.
data WhileConfig
  = Config
    { isHelp :: Bool -- ^ shows help on True, ignoring other configs.
    , err :: Maybe String -- ^ prints an error on Just, ignoring other configs.
    , debugMode :: DebugMode  -- ^ the debug mode.
    , typeCheck :: Bool  -- ^ is type check enabled (not implemented).
    } deriving (Show, Eq)

{-# INLINE defaultConfig #-}
defaultConfig :: WhileConfig
defaultConfig = Config 
  { debugMode = NoDebug
  , typeCheck = False 
  , isHelp = False
  , err = Nothing
  }

-- | Joins options into a configuration.
getConfig :: [WhileOptions] -> WhileConfig
getConfig []
  = defaultConfig
getConfig (o : os) = case o of
  OpDebug t   -> config { debugMode = t }
  OpHelp      -> defaultConfig { isHelp = True }
  OpUnknown e -> defaultConfig { err = Just e }
  where
     config = getConfig os

{-# INLINE usage #-}
usage :: IO ()
usage = do
  T.putStrLn "Usage: runghc main [-h] [--debug=full|step] <while_code.txt>\
            \[<argument_name>=<value>] [...]"
  T.putStrLn "For full support, see\n\
            \https://github.com/sorrowfulT-Rex/50003-Models-of-Computation."

{-# INLINE help #-}
help :: IO ()
help = putStrLn "Run \"runghc main -h\" for help."

-- | The entry point of the While Interpreter CLI.
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
            Just "none" -> OpDebug NoDebug
            Just x      -> OpUnknown x
            _           -> OpDebug StepByStep) "DEBUG") "Debug Options"
        ] args
  if not $ null errs
  then putStr (head errs) >> help -- Print errors
  else do
  -- Parse option arguments
  let config = getConfig ops
  if   isHelp config
  then usage
  else case err config of
    Just e  -> putStrLn ("Unknown parameter " ++ e ++ "!") >> help
    Nothing -> do
    if   null ins
    then help -- No input file
    else do
    -- Parse source code and arguments
    let (src : args) = ins
    case forM args parseArg of
      Left error    -> print error >> help -- Error parsing arguments
      Right context -> do
      -- Handles file-not-find error
      handleDNE ((>> help) . print) $ do
      text <- T.readFile src
      case parseCom $ T.unpack text of
        Left error -> print error -- Error parsing source code
        -- Run the program
        Right com  -> runWhile src config (Context (M.fromList context) []) com

-- | Parses a single argument given to the While code. The arguments are in the
-- form of "v:=E" and serve as the initial context of the program.
parseArg :: String -> Either ParseError (String, Value)
parseArg = flip parse "Argument Parser: " $ do
  eatWSP
  v <- parseIdentifier
  parseReservedOp "=" <|> parseReservedOp "!="
  e <- eval <$> expParser
  case e of
    Right (EVal e) -> return (v, e)
    Left err       -> fail $ "Failed to evaluate " ++ v ++ "! " ++ show err
    _              -> error "UNREACHABLE!!"

-- | Run the While program by the given file name, configuration, context and
-- code. The file name is used for error handling only.
runWhile :: String -> WhileConfig -> Context -> Command -> IO ()
runWhile src
         Config { debugMode = d }
         context
         command = do
  case d of
    FullSteps  -> evalStarPrintS context command -- Small-steps
    StepByStep -> debugWhile     context command -- Interactive debugger
    NoDebug    -> case evalS' context command of -- Big-step
      Right Skip    -> putStrLn "Result: void"
      Right (Ret e) -> putStrLn $ "Result: " ++ show e
      Left err      -> putStrLn $ "Error evaluating " ++ src ++ ".\n"
                    ++ show err
      _             -> error "UNREACHABLE!!"

-- | Interactive debugger.
debugWhile :: Context -> Command -> IO ()
debugWhile ctxt com = introMsg >> putStrLn "" >> go ctxt com 0 True
  where
    -- The brief introduction at the start of the debugger.
    introMsg                 = do
      putStrLn "Press 'x' to dump the context."
      putStrLn "Press 's' to go to the next step."
      putStrLn "Press 'r' to go straight to the result."
      -- putStrLn "Press enter to go to the next line."
      putStrLn "Press 'q' to quit."

    -- Called when the debugger finishes (no error).
    finish ctxt com i        = do
      putStrLn $ "Program completed after " ++ show i ++ " steps!"
      putStrLn $ 
        "Result: " ++ if com == Skip then "void" else drop 7 $ show com
      print ctxt

    -- Called when an error is encountered.
    onError ctxt err         = do
      putStrLn "An error occurs..."
      print err
      print ctxt

    -- Main part of debugger.
    go ctxt com i isPrinting = do
      let debugCycle = do
              -- Get user input (when enabled) or just "r"
              e <- if isPrinting then getLine else return "r"
              if      e `elem` ["x", "dump"] -- Dump context
              then    putStrLn (dumpContext ctxt) >> debugCycle
              else if e `elem` ["s", "step"] -- One step forward
              then    case runStateT (eval1S com) ctxt of
                Left NormalFormError -> finish ctxt com i
                Left err             -> onError ctxt err
                Right (com, ctxt) -> go ctxt com (i + 1) True
              else if e `elem` ["r", "ret", "return"] -- Skip all steps
              then    case runStateT (eval1S com) ctxt of
                Left NormalFormError -> finish ctxt com i
                Left err             -> onError ctxt err
                Right (com, ctxt) -> go ctxt com (i + 1) False
              else if e `elem` ["q", "quit"] -- Quit debugger
              then    return ()
              else    putStrLn "Unrecognised input!" >> introMsg >> debugCycle
      -- Print out the step number
      when isPrinting $ do
        putStrLn $ "Step " ++ show i ++ ":"
        print com
      debugCycle
