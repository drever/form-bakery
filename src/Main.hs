module Main where

import Data.List

main :: IO ()
main = putStrLn "Hello, Haskell!"


data Expr =
      Marked
    | Uncrossed [Expr]
    | Crossed Expr

instance Show Expr where
  show Marked = "<>"
  show (Uncrossed es) = intercalate "" $ map show es
  show (Crossed e) = intercalate "" ["<", show e, ">"]

unmarked = Uncrossed []
marked = Marked

fourthCanon :: Expr
fourthCanon = Uncrossed [Crossed (Uncrossed [Marked, Marked]), Marked]

data Expr' =
      Marked'
    | Uncrossed' [Expr']
    | Crossed' [Expr']

instance Show Expr' where
  show Marked' = "<>"
  show (Uncrossed' es) = intercalate "" $ map show es
  show (Crossed' es) = intercalate "" $ "<" : map show es ++ [">"]

unmarked' = Uncrossed' []

marked' = Crossed' []

fourthCanon' :: Expr'
fourthCanon' = Uncrossed' [Crossed' [Marked', Marked'], Marked']

data Expr'' =
    Unmarked''
  | Call Expr'' Expr''
  | Cross Expr''

instance Show Expr'' where
  show Unmarked'' = ""
  show (Call e1 e2) = show e1 ++ show e2
  show (Cross e) = intercalate "" ["<", show e, ">"]

unmarked'' = Unmarked''
marked'' = Cross Unmarked''

fourthCanon'' :: Expr''
fourthCanon'' = Call (Cross (Call (Cross Unmarked'') (Cross Unmarked''))) (Cross Unmarked'')
fourthCanonA'' = Call
                    (Cross (Call marked'' marked'')) marked''


