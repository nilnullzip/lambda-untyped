module AST where

-- The syntax tree

data Expr =
      Var Char
    | Lambda Char Expr
    | Apply Expr Expr
    | Empty
    | Bound Char Int
  deriving (Show, Eq)

-- Print in De Bruijn index form

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