-- | Main entry point to the application.
module Main where

import Debug.Trace
import Data.Char


-- foo :: String -> String
-- foo "x" = "foo"

data Foo = A | B
    deriving (Show, Eq)

x = (A == B)

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Juan's Lambda calculus interpreter!"
    print (parseLeft EmptyExpression "Lx.Xx")

-- The syntax tree

data Variable = Variable Char
  deriving (Show, Eq)

data Expression =
    Terminal Variable
    | Lambda Variable Expression
    | Application Expression Expression
    | EmptyExpression
  deriving (Show, Eq)

-- Parse expression and return syntax tree

parse :: String -> Expression
parse s = parseLeft EmptyExpression s

-- Left associative expression list

parseLeft :: Expression -> String -> Expression

parseLeft acc s
    | s == "" = trace "parseLeft: EOS" acc
    | acc == EmptyExpression = trace "parseLeft: Begin" (parseLeft t1 r1)
    | otherwise = trace "parseLeft: Application" (parseLeft (Application acc t1) r1)
    where (t1,r1) = parseOne s

-- Parse one term

parseOne :: String -> (Expression, String)

parseOne ('(':s) =
    let (e,r) = break (== ')') s in
    ((parse e) , r)

-- Lambda:

parseOne ('L':x:'.':e) | (isLower x) = (Lambda (Variable x) (parse e), "")

-- Terminal symbol

parseOne (x:r) | (isLower x) = (Terminal (Variable x), r)

-- Syntax error

-- parseOne (s) = trace ("Syntax error at: " ++ s) (EmptyExpression, "")