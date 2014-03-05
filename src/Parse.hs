module Parse where

import           AST
import           Data.Char

-- Literal definitions

identity = "Lz.z"

zero = "Ls.Lz.z"
one  = "Ls.Lz.sz"
two  = "Ls.Lz.s(sz)"
three  = "Ls.Lz.s(s(sz))"
successor = "Lw.Ly.Lx.y(wyx)"
predecessor = "Ln.Lf.Lx.n(Lg.Lh.h(gf))(Lu.x)(Lu.u)"
times = "(Lx.Ly.Lz.x(yz))"

true = "Lx.Ly.x" -- T
false = "Lx.Ly.y" -- F
_and = "Lx.Ly.xyF" -- &
_or = "Lx.Ly.xTy" -- |
_not = "Lx.xFT" -- ~

iszero = "Lx.xF~F" -- Z
gt = "Lx.Ly.Z(xPy)" -- >
eq = "(Lx.Ly.&(Z(xPy))(Z(yPx)))" -- =

yc = "Lf.(Lx.f(xx))(Lx.f(xx))" -- Y combinator

-- Parser

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
parseTerm (x:r) | x == '3' = (parse(three), r)
parseTerm (x:r) | x == 'S' = (parse(successor), r)
parseTerm (x:r) | x == 'P' = (parse(predecessor), r)
parseTerm (x:r) | x == 'M' = (parse(times), r)
parseTerm (x:r) | x == 'Y' = (parse(yc), r)
parseTerm (x:r) | x == 'T' = (parse(true), r)
parseTerm (x:r) | x == 'F' = (parse(false), r)
parseTerm (x:r) | x == '&' = (parse(_and), r)
parseTerm (x:r) | x == '|' = (parse(_or), r)
parseTerm (x:r) | x == '~' = (parse(_not), r)
parseTerm (x:r) | x == 'Z' = (parse(iszero), r)
parseTerm (x:r) | x == '>' = (parse(gt), r)
parseTerm (x:r) | x == '=' = (parse(eq), r)

-- Syntax error:

parseTerm(s) = error ("parseTerm: syntax error: " ++ s)
