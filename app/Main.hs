module Main (main) where

import Parser
import Eval

import System.IO
import qualified Data.Map.Strict as M


main :: IO ()
main = do
    putStrLn "Starting lisp REPL"
    repl (Env (M.empty)) []

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

repl :: Env -> [String] -> IO ()
repl env history = do
    input <- prompt "lisp > "
    case input of
        "exit" -> return ()
        "symbols" -> do
            print env
            repl env history
        "" -> repl env history
        _ -> do
            env' <- evaluateExp env input
            repl env' (input : history)

evaluateExp :: Env -> String -> IO Env
evaluateExp env input = case parseLine input of
    Left e -> do
        putStrLn ("Bad expression: " ++ show e)
        return env
    Right e -> do
        print e
        let (env', val) = eval env e
        print val
        return env'
