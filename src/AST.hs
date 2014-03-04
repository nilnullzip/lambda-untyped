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
    | Lambdam Char Expr
--    | Unbound Char
--    | Closure Char Expr (Map.Map Char Expr)
  deriving (Show, Eq)

-- strip closure for nicer printing

strip :: Expr -> Expr
--strip (Closure x e c) = (Lambda x (strip e))
strip e = e

-- helper

--isNotBound :: Expr -> Bool
--isNotBound (Bound _) = False
--isNotBound _ = True

isLambda :: Expr -> Bool
isLambda (Lambda _ _) = True
isLambda _ = False