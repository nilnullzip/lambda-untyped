module Reduce where

import           AST
import qualified Data.Map    as Map
import           Debug.Trace
import           Parse

-- Binding on stack: name, value, stack depth

data Binding = Binding Char Expr Int deriving (Show)

-- get stack index by variable name

search :: Char -> [Binding] -> Int

search x [] =
    (jtrace ("unbound" ++ show x) )
    0 -- unbound

search x ((Binding y _ i):s)
    | i /= -10 = error ("search: error. found value binding, expected only unbound parameter.")
    | x == y = (jtrace $ "search found: " ++ show x) 1
    | r == 0 = (jtrace $ "search unbound: " ++ show x ) 0
    | otherwise = 1 + r
    where r = search x s

-- get variable value via stack index

get :: Char -> Int -> [Binding] -> (Expr, Int)

get x 1 (b:s) =
    let
        Binding y e j = b
    in
        if x==y then (e, j) else error $ "get: variable names do not match. Looking for: " ++ [x] ++ " got: " ++ [y]

get x i (b:s) = get x (i-1) s

get _ _ _ = error ("get: did not find value")

-- adjust De Bruijn indexes

adjust :: Int -> Int -> Expr -> Expr

adjust d ad (Lambda x e) = Lambda x (adjust (d+1) ad e)

adjust d ad (Apply a b) = Apply (adjust d ad a) (adjust d ad b)

adjust d ad (Bound x 0) = Bound x 0

adjust d ad (Bound x i) =
    jtrace ("adjust: " ++ show x ++ " i: " ++ show i ++ " depth: " ++ show d) $
    if i<=d then Bound x i
    else
        jtrace ("adjust: " ++ show x ++ " " ++ show i ++ " -> " ++ show (i+ad))
        Bound x (i+ad)

adjust _ _ a = error $ "adjust: unexpected term: " ++ show a

-- expression and stack returns expression:

reduce :: Expr -> [Binding] -> Expr

reduce (Var x) s =
    jtrace ("reduce Var: " ++ show x ++ " = " ++ show r)
    r
    where r = Bound x (search x s)

reduce (Bound x i) s =
    let
        -- _ = jtrace ("Bound looking for " ++ show x ++ " at index " ++ show i)
        (g, j) = get x i s
        adj = (length s) - j
        lu = jtrace ("Bound looking for " ++ show x ++ " at index " ++ show i ++ " in stack " ++ show s) $
            if i==0 then (Bound x i) -- unbound
            else if g == Empty then g
            else jtrace ("reduce Bound adj: " ++ show adj) adjust (0) adj g
        r = if lu == Empty then (Bound x i) else lu
    in
    jtrace ("reduce Bound: " ++ show x ++ " " ++ show i ++ " = " ++ show r) $
    r

reduce (Lambda x e) s =
    jtrace ("reduce Lambda: " ++ show x ++ " " ++ show r)
    r
    where
        r = Lambda x (reduce e ((Binding x Empty (-10)) : s))

reduce (Apply f e) s = beta (reduce f s) (reduce e s) s

reduce e s = e

beta (Lambda x f) e s =
    jtrace ("beta: " ++ show x ++ " = " ++ show e ++ " f: " ++ show f)
    adjust 0 (-1) (reduce f s2)
    where 
        s2 = (Binding x e (length s)):s

beta f e s = Apply f e

-- Enable or disable tracing:

--jtrace a b = trace a b
jtrace a b = b

