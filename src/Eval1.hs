module Eval1 where

import qualified Data.Map    as Map
import           Debug.Trace
import           Parse

-- helper

isNotBound :: Expr -> Bool
isNotBound (Bound _) = False
isNotBound _ = True

-- expression and environment returns expression:

eval :: Expr -> (Map.Map Char Expr) -> Expr

-- variable, lookup variable in environment:

eval (Var x) env =
    -- trace ("eval Var: " ++ show (x, env))
    -- (Map.findWithDefault (Unbound x) x env)
    let
        rval = (Map.findWithDefault (Unbound x) x env)
    in
    trace ("eval Var: " ++ show x ++ " = " ++ show rval)
    rval

eval (Bound x) env =
    let
        --rval = (Map.findWithDefault (Bound x) x env)
        lookup = (Map.findWithDefault Empty x env)
        rval = if lookup /= Empty then lookup else trace ("eval Bound: **** lookup failed. **** " ++ show x) (Bound x)
    in
    trace ("eval Bound: " ++ show x ++ " = " ++ show rval)
    rval

eval (Unbound x) env = Unbound x

-- lambda definition, need to find bound and unbound and then return closure:

eval (Lambda x e) env =
    let
        env2 = (Map.insert x (Bound x) env)
        e2 = (eval e env2) -- is this necessary? Can't we just leave unevaluated?
        --rval = (Closure x e2 env2 )
        env3 = Map.filter (isNotBound) env2 -- no bound variables in closure
        rval = (Closure x e2 env3)
    in
    trace ("eval Lambda: " ++ show x ++ " " ++ show e
        ++ "\n  env: " ++ show env
        ++ "\n  env3: " ++ show env3
        ++ "\n  rval: " ++ show rval)
    rval
    -- (Lambda x (eval e (Map.insert x (Bound x) env))  )

-- evaluate closure.

eval (Closure x e c) env =
    let
        cenv = Map.insert x (Bound x) (Map.union c env)
        --env2 = Map.insert x (Bound x) env
        --e2 = (eval e env2)
        --rval = (Closure x e2 env)
        cenv2 = Map.filter (isNotBound) cenv -- eliminate any bound variables in closure
        e2 = (eval e cenv)
        rval = (Closure x e2 cenv2)
    in
    trace ("eval Closure: " ++ (show x)
        ++ "\n  e: " ++ show e
        -- ++ "\n  env: " ++ (show (Map.findWithDefault Empty x env2))
        ++ "\n  c: " ++ (show c)
        ++ "\n  cenv: " ++ (show cenv)
        ++ "\n  returns: " ++ show (strip rval))
    rval

-- eval (Closure x e c) env = Closure x e c

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

beta (Apply (Closure x e c) a) env =
    let
        env2 = (Map.insert x (eval a env) (Map.union c env))
        rval = (eval e env2)
    in
    trace ("beta: " ++ show x ++ " = " ++ show a
        ++ "\n  e: " ++ show e
        ++ "\n  c: " ++ show c
        ++ "\n  env2: " ++ show env2
        ++ "\n  returns: " ++ show (strip rval) )
    rval

--beta (Apply (Lambda x e) a) env =
--    -- trace ("lambdac application: " ++ show (Map.insert x (eval a env) env))
--    (eval e (Map.insert x (eval a env) env))

-- otherwise no further reduction

beta e env = e
--beta e env = error ("beta called with no possible match.")

-- strip closure for nicer printing

strip :: Expr -> Expr
strip (Closure x e c) = (Lambda x (strip e))
strip e = e
