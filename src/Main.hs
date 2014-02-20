-- | Main entry point to the application.

module Main where

import Debug.Trace
import Data.Char
import System.Exit
import qualified Data.Map as Map

-- | The Lambda expression to parse

theLambda = "S(S0)"
--theLambda = "2"

-- | Literal definitions

identity = "Lz.z"
zero = "Ls.Lz.z"
one  = "Ls.Lz.sz"
two  = "Ls.Lz.s(sz)"
successor = "Lw.Ly.Lx.y(wyx)"

-- | The syntax tree

data Expr =
      Var Char
    | Lambda Char Expr
    | Apply Expr Expr
  deriving (Show, Eq)

-- | Parse and return syntax tree

parse :: String -> Expr
parse s
    | s == "" = error ("parse: syntax error: empty string")
    | otherwise = parseApply t1 r1
    where (t1,r1) = parseTerm s

-- | Left associative expression application

parseApply :: Expr -> String -> Expr

parseApply acc s
    | s == "" = {- trace "parseLeft: EOS" -} acc
    | otherwise = {- trace "parseLeft: Apply" -} (parseApply (Apply acc t1) r1)
    where (t1,r1) = parseTerm s

-- | Parse a term

parseTerm :: String -> (Expr, String)

-- | Parens:

parseTerm ('(':s)
    | r /= "" && (head r) == ')' = ((parse e) , tail r)
    where (e,r) = break (== ')') s
--    error ("parseTerm: e,r = " ++ e ++ " " ++ r)

-- | Lambda:

parseTerm ('L':x:'.':e) | (isLower x) = (Lambda x (parse e), "")

-- | Variable:

parseTerm (x:r) | (isLower x) = (Var x, r)

-- | Literals :

parseTerm (x:r) | x == 'I' = (parse(identity), r)
parseTerm (x:r) | x == '0' = (parse(zero), r)
parseTerm (x:r) | x == '1' = (parse(one), r)
parseTerm (x:r) | x == '2' = (parse(two), r)
parseTerm (x:r) | x == 'S' = (parse(successor), r)

-- | Syntax error:

parseTerm(s) = error ("parseTerm: syntax error: " ++ s)

-- | Evaluate

beta :: Expr -> (Map.Map Char Expr) -> Expr
beta (Apply (Lambda x e) a) env = eval e (Map.insert x (eval a env) env)
beta e env = e

eval :: Expr -> (Map.Map Char Expr) -> Expr
eval (Var x) env = {- trace ("eval: " ++ show x) -} Map.findWithDefault (Var x) x env 
eval (Lambda x e) env = {- trace ("eval: lambda " ++ show x) -} (Lambda x (eval e env))
eval (Apply a b) env = {- trace ("eval: apply") -} beta (Apply (eval a env) (eval b env)) env

-- | The main entry point.

main :: IO ()
main = do
    putStrLn "Juan's Lambda calculus interpreter!"
    putStrLn ""
    putStrLn "The lambda expression:"
    print theLambda
    putStrLn ""
    putStrLn "The AST:"
    print (parse theLambda)
    putStrLn ""
    putStrLn "Fully reduced AST:"
    print (eval (parse theLambda) Map.empty)
