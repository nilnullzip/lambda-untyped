-- Untyped Lambda Calculus interpreter
--------------------------------------

module Main where

import qualified Data.Map    as Map
import           Debug.Trace
import           Eval1
import           Parse

-- The Lambda expression to parse

expected  = "Lx.(Ly.yy)(Lu.x)"
theLambda = "Lx.(Lx.xx)(Lu.x)"

-- The entry point

main :: IO ()
main = do
    putStrLn "Juan's Lambda calculus interpreter!"
    putStrLn ""
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
    print (strip (eval (parse expected) Map.empty))
    putStrLn ""
    putStrLn "Fully reduced AST:"
    print (strip (eval (parse theLambda) Map.empty))
