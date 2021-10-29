{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Console.GetOpt
import System.Environment
import Control.Monad
import Utilities ( forMBreak )
import Gadgets.IO ( handleDNE )
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Command
import Text.Parsec
import Expression
import Definitions

data WhileOptions 
  = OpHelp
  deriving Show

usage :: IO ()
usage = do 
  T.putStrLn "Usage: runghc main <while_code.txt>"
  T.putStrLn "For full support, see\n\
            \https://github.com/sorrowfulT-Rex/50003-Models-of-Computation."

help :: IO ()
help = do 
  putStrLn "Run \"runghc main -h\" to for help."

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
      handleDNE ((>> help) . print) $ do
        text <- T.readFile src
        case parseCom $ T.unpack text of
          Left error -> print error
          Right com  -> do
            putStr "result: "
            case eval com of
              Just Skip ->    putStrLn "void"
              Just (Ret e) -> print e
              _         ->    putStrLn "Evaluation Error!"
