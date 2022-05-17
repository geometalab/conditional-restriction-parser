{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import System.Console.CmdArgs
import ConditionalRestriction (Result(Ok, Err), evaluate, ID, Type (TBool, TNum, TTime), parseRestriction, needsData)
import System.Exit (exitFailure)
import Control.Monad (when, unless)

data Program
  = Evaluate {restriction :: String, values :: [String]}
  | Parse {restriction :: String}
  | DataNeeded {restriction :: String}
  deriving (Show, Data, Typeable, Eq)

programModes :: Mode (CmdArgs Program)
programModes =
  cmdArgsMode $
    modes
      [ Evaluate
          { restriction = def &= typ "RESTRICTION" &= argPos 0,
            values = def &= typ "KEY=VALUE" &= args
          }
          &= auto
          &= help "Evaluate a conditional restriction based on data given in key=value format. To check the needed data use [data-needed].",
        Parse
          { restriction = def &= typ "PRESTRICTION" &= argPos 0
          }
          &= help "Parse a conditional restriction and return its AST.",
        DataNeeded
          { restriction = def &= typ "DRESTRICTION" &= argPos 0
          }
          &= name "data-needed"
          &= help "List the data needed to evaluate this conditional restriction."
      ]
      &= program "conditional-restriction-parser-exe"
      &= summary "Conditional Restriction Parser v0.1.0.0, (C) Lukas Buchli 2022"

main :: IO ()
main = cmdArgsRun programModes >>= \case
  Evaluate r kvs -> do
    kvs' <- mapM separate kvs
    case evaluate r kvs' of
      Ok Nothing -> putStrLn "This restriction does not match the given data. Output unknown."
      Ok (Just t) -> putStrLn t
      Err (msgs, neededs) -> do
        unless (null msgs) $ do
          putStrLn "Errors were encountered during evaluation:"
          mapM_ (putStrLn . (" - " ++)) msgs
        unless (null neededs) $ do
          putStrLn "The following data is required for evaluation but has not been provided:"
          mapM_ (putStrLn . (" - " ++) . showData) neededs
        exitFailure
  Parse r -> case parseRestriction r of
    Ok ast -> print ast
    Err msg -> do
      putStrLn msg
      exitFailure
  DataNeeded r -> case needsData r of
    Ok neededs -> mapM_ (putStrLn . (" - " ++) . showData) neededs
    Err msg -> do
      putStrLn msg
      exitFailure
 where
   separate s = case split '=' s of
     [k,v] -> return (k, v)
     _ -> do
       putStrLn $ "Expected " ++ s ++ " to be of format KEY=VALUE."
       exitFailure

split d (x:xs) | d == x = [] : split d xs
split d (x:xs) = case split d xs of
                   (xs':xss) -> (x:xs'):xss
                   [] -> [[x]]
split d [] = []

showData :: (ID, Type) -> String
showData (id, TBool) = id ++ ": BOOL (e.g. '" ++ id ++ "=false')"
showData (id, TNum) = id ++ ": NUM (e.g. '" ++ id ++ "=3.0')"
showData (id, TTime) = id ++ ": TIME (e.g. '" ++ id ++ "=\"2022-05-10 12:35\"'"
