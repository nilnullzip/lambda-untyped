-- Untyped Lambda Calculus interpreter
--------------------------------------

module Main where

import           AST
import qualified Data.Map    as Map
import           Debug.Trace
import           Reduce
import           Parse
import Test

-- The Lambda expression to parse

expected  = "Ly.y"
theLambda = "(Lx.xx)Ly.y"

-- The entry point

main :: IO ()
main = do
    putStrLn "Juan's Lambda calculus interpreter!"
    putStrLn ""
    runtests

{-
foo = do
    putStrLn "The lambda expression:"
    print theLambda
    putStrLn ""
    putStrLn "The expected AST:"
    print (parse expected)
    putStrLn ""
    putStrLn "The AST:"
    print (parse theLambda)
    putStrLn ""
    putStrLn "The expected reduced AST:"
    print (strip (reduce (parse expected) []))
    putStrLn ""
    putStrLn "Fully reduced AST:"
    print (strip (reduce (parse theLambda) []))
-}