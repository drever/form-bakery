{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reflex.Dom
import Data.List

import qualified Data.Text as T

main :: IO ()
main = mainWidget $ do
           text "Hello, Spencer Brown!"
           text (T.pack . show $ theorem3)

xx = show theorem3

-- | 1
-- | The form

data Distinction = PerfectContinence

-- | 2
-- | Froms taken out of the form

-- | Expression

data Expr =
    Unmarked
  | Cross Expr
  | Call Expr Expr

instance Show Expr where
  show Unmarked = ""
  show (Call e1 e2) = show e1 ++ show e2
  show (Cross e) = intercalate "" ["<", show e, ">"]

unmarked = Unmarked
marked = Cross Unmarked

-- | Primitive equation

primitive1 (Call (Cross Unmarked) (Cross Unmarked)) = Call (Cross Unmarked) Unmarked
primitive1 e = e

primitive2 (Cross (Cross Unmarked)) = Unmarked
primitive2 e = e

-- | Simple expressions

simpleExpressions = [
        Call marked marked
      , Cross marked
      , Cross Unmarked
      , Unmarked]

-- | Operation
cross = Cross

-- | Relation

continence :: Expr -> Expr -> Bool
continence = undefined

-- | 3
-- | The conception of calculation

-- | Fourth canon. Hypothesis of simplification

fourthCanon :: Expr
fourthCanon = Call (Cross (Call (Cross Unmarked) (Cross Unmarked))) (Cross Unmarked)
fourthCanon' = Call (Cross (Call marked marked)) marked

theorem3 =
  Call (Cross
     (Call (Cross
         (Call
               (Cross (Call marked marked))
               (marked)))
        (Cross marked)))
    (Cross (Cross (Cross marked)))

data AlgExpr = AlgExpr

-- | Appendix 2
-- | The calculus interpreted as logic

