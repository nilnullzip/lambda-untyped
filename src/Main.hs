-- | Main entry point to the application.
module Main where

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"

-- The syntax tree

data Variable = Variable Char
  deriving Show

data Expression = Terminal Variable
    | Lambda Variable Expression
    | Application Expression Expression
  deriving Show

-- Parse expression

parse :: String -> Expression

-- Lambda:

parse ('L':x:'.':e) = Lambda (Variable x) (parse e)

-- Terminal

parse (x:[]) = Terminal (Variable x)

-- Application

-- parse (x:e)