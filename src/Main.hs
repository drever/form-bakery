module Main where

import Data.List

main :: IO ()
main = putStrLn "Hello, Haskell!"


data Expr =
    Unmarked
  | Call Expr Expr
  | Cross Expr

instance Show Expr where
  show Unmarked = ""
  show (Call e1 e2) = show e1 ++ show e2
  show (Cross e) = intercalate "" ["<", show e, ">"]

unmarked = Unmarked
marked = Cross Unmarked

primitive1 (Call (Cross Unmarked) (Cross Unmarked)) = Call (Cross Unmarked) Unmarked
primitive1 e = e

primitive2 (Cross (Cross Unmarked)) = Unmarked
primitive2 e = e

fourthCanon :: Expr
fourthCanon = Call (Cross (Call (Cross Unmarked) (Cross Unmarked))) (Cross Unmarked)
fourthCanon' = Call (Cross (Call marked marked)) marked


data AlgExpr = AlgExpr

