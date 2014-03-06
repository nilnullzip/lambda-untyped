-- Untyped Lambda Calculus interpreter
--------------------------------------

module Main where

import           AST
import           Parse
import           Reduce
import           Test
import System.IO
import Help

-- The entry point

main :: IO ()
main = do
    putStrLn "\n\nJuan's Lambda calculus interpreter!"
    putStrLn ""
    putStrLn help
    repl

prompt :: String -> IO String
prompt p = do
    putStr p
    hFlush stdout
    getLine

repl :: IO ()
repl = do
    l <- prompt "> "
    if l=="" then repl
    else if l=="?" then putStrLn help
    else if l=="?t" then runtests
    else do
        putStrLn $ "De Bruijn:   " ++ (pdbi (reduce (parse l) []))
        putStrLn ""
        putStrLn $ "Reduced AST: " ++ (show (reduce (parse l) []))
        putStrLn ""
        putStrLn $ "Initial AST: " ++ (show (parse l))
    repl

