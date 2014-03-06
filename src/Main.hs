-- Untyped Lambda Calculus interpreter
--------------------------------------

module Main where

import           AST
import           Parse
import           Reduce
import           Test
import System.IO

-- The entry point

main :: IO ()
main = do
    putStrLn "\n\nJuan's Lambda calculus interpreter!"
    putStrLn ""
    help
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
    else if l=="?" then help
    else if l=="?t" then runtests
    else do
        putStrLn $ "De Bruijn:   " ++ (pdbi (reduce (parse l) []))
        putStrLn ""
        putStrLn $ "Reduced AST: " ++ (show (reduce (parse l) []))
        putStrLn ""
        putStrLn $ "Initial AST: " ++ (show (parse l))
    repl

help :: IO ()
help = do
    putStrLn "type '?' for help"
    putStrLn "otherwise type in a lambda expression."
    putStrLn ""
    putStrLn "Input format: Lf.Lx.fx"
    putStrLn ""
    putStrLn "Variables names are single character."
    putStrLn ""
    putStrLn "Literals:"
    putStrLn "  numbers:    0 1 2 3 4 5 6 7 8 9"
    putStrLn "  arithmetic: + - * ^"
    putStrLn "  inequality: Z > < ="
    putStrLn "  logical:    T F & | ~"
    putStrLn "  pairs:      C A D"
    putStrLn "  recursion:  Yf = f(Yf)"
    putStrLn "  identity:   If = f"
    putStrLn ""
    putStrLn "Examples:"
    putStrLn "  Add two numbers '1+2'"    
    putStrLn "  Multiply two numbers '*23'"    
    putStrLn "  Sum the first three integers: 'Y(Lr.Ln.Zn0(n+(r(-n))))3'"
    putStrLn ""
    putStrLn "Results are printed in De Bruijn format: L.L.21"
    putStrLn ""
    putStrLn "Random:"
    putStrLn "  type '?t' to run regression tests"
    putStrLn ""
