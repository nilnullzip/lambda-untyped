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

pdbi (Apply a b) = "(" ++ pdbi a ++ ")(" ++ pdbi b ++ ")"
