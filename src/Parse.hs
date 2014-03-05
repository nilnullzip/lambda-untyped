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
gt = "Lx.Ly.Z(x-(+y))" -- >
lt = "Lx.Ly.Z(y-(+x))" -- <
ge = "Lx.Ly.Z(x-y)"
eq = "(Lx.Ly.&(Z(x-y))(Z(y-x)))" -- =

yc = "Lf.(Lx.f(xx))(Lx.f(xx))" -- Y combinator

cons = "Lx.Ly.Lf.fxy"
car = "Lp.pT"
cdr = "Lp.pF"

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
parseTerm (x:r) | x == 'Y' = (parse(yc), r)

parseTerm (x:r) | x == '0' = (parse(zero), r)
parseTerm (x:r) | x == '1' = (parse(one), r)
parseTerm (x:r) | x == '2' = (parse(two), r)
parseTerm (x:r) | x == '3' = (parse(three), r)
parseTerm (x:r) | x == '4' = (parse("+3"), r)
parseTerm (x:r) | x == '5' = (parse("+4"), r)
parseTerm (x:r) | x == '6' = (parse("+5"), r)
parseTerm (x:r) | x == '7' = (parse("+6"), r)
parseTerm (x:r) | x == '8' = (parse("+7"), r)
parseTerm (x:r) | x == '9' = (parse("+8"), r)

parseTerm (x:r) | x == '+' = (parse(successor), r)
parseTerm (x:r) | x == '-' = (parse(predecessor), r)
parseTerm (x:r) | x == '*' = (parse(times), r)

parseTerm (x:r) | x == 'T' = (parse(true), r)
parseTerm (x:r) | x == 'F' = (parse(false), r)
parseTerm (x:r) | x == '&' = (parse(_and), r)
parseTerm (x:r) | x == '|' = (parse(_or), r)
parseTerm (x:r) | x == '~' = (parse(_not), r)

parseTerm (x:r) | x == 'Z' = (parse(iszero), r)
parseTerm (x:r) | x == '>' = (parse(gt), r)
parseTerm (x:r) | x == '<' = (parse(lt), r)
parseTerm (x:r) | x == '=' = (parse(eq), r)

parseTerm (x:r) | x == 'C' = (parse(cons), r)
parseTerm (x:r) | x == 'A' = (parse(car), r)
parseTerm (x:r) | x == 'D' = (parse(cdr), r)

-- Syntax error:

parseTerm(s) = error ("parseTerm: syntax error: " ++ s)
