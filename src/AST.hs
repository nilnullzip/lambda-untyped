module AST where

import           Data.Char
import qualified Data.Map  as Map

-- The syntax tree

data Expr =
      Var Char
    | Lambda Char Expr
    | Apply Expr Expr
    | Empty
    | Bound Char Int
  deriving (Show, Eq)

-- strip closure for nicer printing

strip :: Expr -> Expr
strip e = e

-- Print De Bruijn index forms

pdbi :: Expr -> String

pdbi (Bound x i) =
    let
        is = show i
    in
        if i==0 then [x]
        else if length(is) == 1 then is
        else "{" ++ is ++ "}"

pdbi (Lambda x e) = "L." ++ pdbi e

pdbi (Apply a b) = paren a ++ paren b

paren :: Expr -> String

paren (Bound x i) = pdbi (Bound x i)

paren (Lambda x e) = pdbi (Lambda x e)

paren e = "(" ++ pdbi e ++ ")"