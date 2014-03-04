module Reduce where

import           AST
import qualified Data.Map    as Map
import           Debug.Trace
import           Parse

data Binding = Binding Char Expr deriving (Show)

-- get stack index for variable

search :: Char -> [Binding] -> Int
search x [] =
    (trace ("unbound" ++ show x) )
     0 -- unbound
search x ((Binding y _):s)
    | x == y = (trace $ "search found: " ++ show x ) 1
    | r == 0 = (trace $ "search unbound: " ++ show x ) 0
    | otherwise = 1 + r
    where r = search x s

-- get variable value via stack index

get :: Char -> Int -> [Binding] -> Expr
get x 1 (b:s) =
    let
        Binding y e = b
    in
        if x==y then e else error $ "get: variable names do not match. Looking for: " ++ [x] ++ " got: " ++ [y]
get x i (b:s) = get x (i-1) s
get _ _ _ = error ("get: did not find value")

-- expression and stack returns expression:

reduce :: Expr -> [Binding] -> Expr

reduce (Var x) s =
    trace ("reduce Var: " ++ show x ++ " = " ++ show r)
    r
    where r = Bound x (search x s)

reduce (Bound x i) s =
    let
        lu = if i==0 then (Bound x i) else get x i s
        r = if lu == Empty then (Bound x i) else lu
    in
    trace ("reduce Bound: " ++ show x ++ " " ++ show i ++ " = " ++ show r) $
    r

reduce (Lambda x e) s =
    trace ("reduce Lambda: " ++ show x ++ " " ++ show r)
    r
    where
        r = Lambda x (reduce e (Binding x Empty : s))

reduce (Apply f e) s = beta (reduce f s) (reduce e s) s

reduce e s = e

beta (Lambda x f) e s =
    trace ("beta: " ++ show x ++ " = " ++ show e ++ " f: " ++ show f)
    reduce f s2
    where 
        s2 = (Binding x e):s

beta f e s = Apply f e

--beta f e s

{-
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

-}