-- Lambda Calculus interpreter
------------------------------

module Main where

import Debug.Trace
import Data.Char
import System.Exit
import qualified Data.Map as Map

-- *** The Lambda expression to parse ***

{-

Eval problems:

1) P1 works, but PS0 does not. It's because of the difference in variable names between 1 and S0.

Fundumental reason:

theLambda = "(Lx.Lx.x)z" -- wrong = Lx.z
theLambda = "(Ly.Lx.x)z" -- right = Lx.x

In the first case, outer Lx substitutes z in the inner lambda expression, which is incorrect.
This is because the evaluation of Lx.x does not include binding of x in the unapplied lambda expression case.

2) When argment is passed, it should use a closure for values in its scope. I currently
just blindly substitue, which gives wrong result:

theLambda = "(Lx.Lz.xz)z" -- wrong = Lz.zz
theLambda = "(Lx.Ly.xy)z" -- right = Ly.zy

-}

theLambda = "(Lx.Lx.x)z" -- wrong 
-- theLambda = "(Lx.Ly.xy)z" -- right
-- theLambda = "P(Ly.Lx.yx)"
-- theLambda = "2"

-- Literal definitions

identity = "Lz.z"
zero = "Ls.Lz.z"
one  = "Ls.Lz.sz"
two  = "Ls.Lz.s(sz)"
successor = "Lw.Ly.Lx.y(wyx)"
predecessor = "Ln.Lf.Lx.n(Lg.Lh.h(gf))(Lu.x)(Lu.u)"

-- The syntax tree

data Expr =
      Var Char
    | Lambda Char Expr
    | Apply Expr Expr
    | Empty
  deriving (Show, Eq)

-- Parse and return AST
-----------------------

parse :: String -> Expr
parse s
    | s == "" = error ("parse: syntax error: empty string")
    | otherwise = fst (parseApply t r)
    where (t,r) = parseTerm s

-- Left associative expression application

parseApply :: Expr -> String -> (Expr, String)

parseApply acc s
    | s == "" = {- trace "parseLeft: EOS" -} (acc, "")
    | head s == ')' = (acc, s)
    | acc == Empty = (parseApply t r)
    | otherwise = {- trace "parseLeft: Apply" -} (parseApply (Apply acc t) r)
    where (t,r) = parseTerm s

-- Parse a term

parseTerm :: String -> (Expr, String)

-- Parens:

parseTerm ('(':s)
    | r == "" = error ("parseTerm: empty string after left paren " ++ show e)
    | head r == ')' = (e, tail r)
    | otherwise = error ("parseTerm: missing right paren: " ++ r)
    where (e,r) = parseApply Empty s

--    | r /= "" && (head r) == ')' = ((parse e) , tail r)
--    where (e,r) = break (== ')') s
--    error ("parseTerm: e,r = " ++ e ++ " " ++ r)

-- Lambda:

parseTerm ('L':x:'.':e) | (isLower x) =
    let (exp, r) = parseApply Empty e in (Lambda x exp, r)

-- Variable:

parseTerm (x:r) | (isLower x) = (Var x, r)

-- Literals :

parseTerm (x:r) | x == 'I' = (parse(identity), r)
parseTerm (x:r) | x == '0' = (parse(zero), r)
parseTerm (x:r) | x == '1' = (parse(one), r)
parseTerm (x:r) | x == '2' = (parse(two), r)
parseTerm (x:r) | x == 'S' = (parse(successor), r)
parseTerm (x:r) | x == 'P' = (parse(predecessor), r)

-- Syntax error:

parseTerm(s) = error ("parseTerm: syntax error: " ++ s)

-- Evaluate AST
---------------

eval :: Expr -> (Map.Map Char Expr) -> Expr
eval (Var x) env = {- trace ("eval: " ++ show x) -} Map.findWithDefault (Var x) x env 
eval (Lambda x e) env = {- trace ("eval: lambda " ++ show x) -} (Lambda x (eval e env))
eval (Apply a b) env = {- trace ("eval: apply") -} beta (Apply (eval a env) (eval b env)) env

beta :: Expr -> (Map.Map Char Expr) -> Expr
beta (Apply (Lambda x e) a) env = eval e (Map.insert x (eval a env) env)
beta e env = e

-- The main entry point
-----------------------

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
