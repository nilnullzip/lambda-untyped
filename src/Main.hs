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

expected = "P((Lw.Ly.Lc.y(wyc))0)" -- good result
theLambda =  "P((Lw.Ly.Lx.y(wyx))0)" -- bad result

Simplified:

expected  = "Lx.(Lc.cf)(Lu.x)" -- good
theLambda = "Lx.(Lx.xf)(Lu.x)" -- bad

Still some problem with variable naming. Substituting c for x above works correctly.

-}

-- expected =  "(Lw.Ly.Lx.y(wyx))0"
-- expected =  "P((Lw.Ly.Lx.y(wyx))0)" -- bad result
-- theLambda = "P((Lw.Ly.Lc.y(wyc))0)" -- good result

--expected  = "Lf.Lx.(Ly.Lc.yc)(Lg.Lh.h(gf))(Lu.x)(Lu.u)" -- good result
--theLambda = "Lf.Lx.(Ly.Lx.yx)(Lg.Lh.h(gf))(Lu.x)(Lu.u)" -- bad result

--theLambda = "P1"
--expected  = "Lx.(Lx.(Lg.Lh.h(gf))x)(Lu.x)"
--theLambda = "Lx.(Lc.(Lg.Lh.h(gf))c)(Lu.x)"

-- expected  = "Lx.(Lc.cf)(Lu.x)" -- good
-- theLambda = "Lx.(Lx.xf)(Lu.x)" -- bad

-- expected =  "Lc.(Ls.Lx.s)(Lu.c)(Lu.u)"
-- theLambda = "Lx.(Ls.Lx.s)(Lu.x)(Lu.u)" -- fail

expected = "2"
theLambda = "S(S0)"

--theLambda = "(Ln.Lf.Lc.n(Lg.Lh.h(gf))(Lu.c)(Lu.u))(Ls.Lx.sx)" -- good

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
    | Unbound Char
    | Bound Char
    | Lambdac Char Expr (Map.Map Char Expr)
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

-- expression and environment returns expression:

eval :: Expr -> (Map.Map Char Expr) -> Expr

-- variable, lookup variable in environment:

eval (Var x) env =
    -- trace ("eval Var: " ++ show (x, env))
    -- (Map.findWithDefault (Unbound x) x env)
    let
        rval = (Map.findWithDefault (Unbound x) x env) -- shouldn't really have a default value
    in
    trace ("eval Var: " ++ show x ++ " = " ++ show rval)
    rval 

eval (Bound x) env =
    let
        rval = (Map.findWithDefault (Unbound x) x env) -- shouldn't really have a default value
    in
    trace ("eval Bound: " ++ show x ++ " = " ++ show rval)
    rval 

eval (Unbound x) env = Unbound x

-- lambda definition, insert variable in environment, evaluate body, return closure:

eval (Lambda x e) env =
    let
        env2 = (Map.insert x (Bound x) env)
        rval = (Lambdac x (eval e env2) env )
    in
    trace ("eval Lambda: " ++ show x ++ " " ++ show e
        ++ "\n  env: " ++ show env
        ++ "\n  rval: " ++ show rval)
    rval
    -- (Lambda x (eval e (Map.insert x (Bound x) env))  )

-- evaluate closure. *** Not clear whether closure or passed environment should be used here! Both work for different cases.

eval (Lambdac x e c) env =
    let
        cenv = Map.union c env
        env2 = Map.insert x (Bound x) env
        e2 = (eval e env2)
        rval = (Lambdac x e2 env)
    in
    trace ("eval Lambdac: " ++ (show x) ++ (show e)
        ++ "\n  e2: " ++ show e2
        ++ "\n  env: " ++ (show (Map.findWithDefault (Var 'Z') x env2))
        ++ "\n  returns: " ++ show (strip rval))
    rval

-- application, evaluate both parts then call beta reduction

eval (Apply a b) env =
    trace ("eval apply: "
        ++ "\n  " ++ show a
        ++ "\n  " ++ show b)
    beta (Apply (eval a env) (eval b env)) env

eval e env =
    error ("eval: cannot match: " ++ show e)

-- beta reduction, i.e. application

beta :: Expr -> (Map.Map Char Expr) -> Expr

-- if lambda application, then variable substituation and evaluate:

beta (Apply (Lambdac x e closure) a) env =
    let
        env2 = (Map.insert x (eval a env) (Map.union closure env))
        rval = (eval e env2)
    in
    trace ("beta: " ++ show x ++ " = " ++ show e
        ++ "\n  env: " ++ show env2
        ++ "\n  returns: " ++ show (strip rval) )
    --(eval e (Map.insert x (eval a env) (Map.union closure Map.empty)))
    rval
    --(eval e (Map.insert x (eval a env) env))

--beta (Apply (Lambda x e) a) env =
--    -- trace ("lambdac application: " ++ show (Map.insert x (eval a env) env))
--    (eval e (Map.insert x (eval a env) env))

-- otherwise no further reduction

beta e env = e
--beta e env = error ("beta called with no possible match.")

-- strip closure for nicer printing

strip :: Expr -> Expr
strip (Lambdac x e c) = (Lambda x (strip e))
strip e = e

-- The main entry point
-----------------------

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
