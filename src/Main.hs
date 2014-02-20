-- | Main entry point to the application.

module Main where


import Debug.Trace
import Data.Char
import System.Exit

-- | The Lambda expression to parse

identity = "Lz.z"
zero = "Ls.Lz.z"
one  = "Ls.Lz.s(z)"
two  = "Ls.Lz.s(s(z))"
successor = "Lw.Ly.Lx.y(wyx)"

--theLambda = "Lx.xyzLa.ab"
theLambda = "S"

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Juan's Lambda calculus interpreter!"
    print (parse theLambda)

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
    | s == "" = trace "parseLeft: EOS" acc
    | otherwise = trace "parseLeft: Apply" (parseApply (Apply acc t1) r1)
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
parseTerm (x:r) | x == 'S' = (parse(successor), r)

-- | Syntax error:

parseTerm(s) = error ("parseTerm: syntax error: " ++ s)
