module Parse where

import           AST
--import           Data.Char

isLower c = 'z' >= c && c >= 'a'

-- Parser

parse :: String -> Expr

parse "" = error ("parse: syntax error: empty string")

parse s = fst (parseApply t r)
    where (t,r) = parseTerm s

-- Left associative expression application

parseApply :: Expr -> String -> (Expr, String)

parseApply acc "" = {- trace "parseLeft: EOS" -} (acc, "")

parseApply acc (')':s) = (acc, ')':s)

parseApply acc s
    | acc == Empty = (parseApply t r)
    | otherwise = {- trace "parseLeft: Apply" -} (parseApply (Apply acc t) r)
    where (t,r) = parseTerm s

-- Parse a term

parseTerm :: String -> (Expr, String)

--parseTerm s = error ("parseTerm: string=" ++ s)

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

-- Literals

parseTerm (x:r) | x == 'I' = (parse("Lz.z"), r) -- identity
parseTerm (x:r) | x == 'Y' = (parse("Lf.(Lx.f(xx))(Lx.f(xx))"), r) -- Y combinator

-- Literals: Numbers. May also be applied to a function to apply multiple times

parseTerm (x:r) | x == '0' = (parse("Ls.Lz.z"), r)
parseTerm (x:r) | x == '1' = (parse("Ls.Lz.sz"), r)
parseTerm (x:r) | x == '2' = (parse("Ls.Lz.s(sz)"), r)
parseTerm (x:r) | x == '3' = (parse("Ls.Lz.s(s(sz))"), r)
parseTerm (x:r) | x == '4' = (parse("+3"), r)
parseTerm (x:r) | x == '5' = (parse("+4"), r)
parseTerm (x:r) | x == '6' = (parse("+5"), r)
parseTerm (x:r) | x == '7' = (parse("+6"), r)
parseTerm (x:r) | x == '8' = (parse("+7"), r)
parseTerm (x:r) | x == '9' = (parse("+8"), r)

-- Literals: arithmetic

parseTerm (x:r) | x == '+' = (parse("Lw.Ly.Lx.y(wyx)"), r) -- successor/addition
parseTerm (x:r) | x == '-' = (parse("Ln.Lf.Lx.n(Lg.Lh.h(gf))(Lu.x)(Lu.u)"), r) -- predecessor/subtraction
parseTerm (x:r) | x == '*' = (parse("Lx.Ly.Lz.x(yz)"), r) -- multiplication (prefix)
parseTerm (x:r) | x == '^' = (parse("Lx.Ly.yx"), r) -- exponentiation (prefix)

-- Literals: Logic

parseTerm (x:r) | x == 'T' = (parse("Lx.Ly.x"), r) -- True
parseTerm (x:r) | x == 'F' = (parse("Lx.Ly.y"), r)
parseTerm (x:r) | x == '&' = (parse("Lx.Ly.xyF"), r)
parseTerm (x:r) | x == '|' = (parse("Lx.Ly.xTy"), r)
parseTerm (x:r) | x == '~' = (parse("Lx.xFT"), r)

-- Literals: inequalities

parseTerm (x:r) | x == 'Z' = (parse("Lx.xF~F"), r) -- is zero
parseTerm (x:r) | x == '>' = (parse("Lx.Ly.Z(x-(+y))"), r)
parseTerm (x:r) | x == '<' = (parse("Lx.Ly.Z(y-(+x))"), r)
parseTerm (x:r) | x == 'G' = (parse("Lx.Ly.Z(x-y)"), r) -- Greater than or equal
parseTerm (x:r) | x == '=' = (parse("(Lx.Ly.&(Z(x-y))(Z(y-x)))"), r)

-- lLiterals: ists

parseTerm (x:r) | x == 'C' = (parse("Lx.Ly.Lf.fxy"), r) -- cons
parseTerm (x:r) | x == 'A' = (parse("Lp.pT"), r) -- car
parseTerm (x:r) | x == 'D' = (parse("Lp.pF"), r) -- cdr

-- Syntax error:

parseTerm(s) = error ("parseTerm: syntax error: " ++ s)
