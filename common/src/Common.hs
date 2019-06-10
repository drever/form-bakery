module Common where

import Reflex.Dom
import Data.List

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

instance Semigroup Expr where
   e1 <> e2 = Call e1 e2

instance Monoid Expr where
   mempty = Unmarked

instance Read Expr where
   readsPrec _ = undefined

-- "<>" -> Cross Unmarked
-- "<><>" -> Call (Cross Unmarked) (Cross Unmarked)
-- "<<>>" -> Cross (Cross Unmarked)
-- "<><><>" -> Call (Cross Unmarked) (Call (Cross Unmarked) (Cross Unmarked))
-- "<<><>>" -> Cross (Call (Cross Unmarked) (Cross Unmarked))
-- "<<>><>" -> Call (Cross (Cross Unmarked)) (Cross Unmarked)

parseExpr :: String -> Expr
parseExpr xs = parseExpr' (0, 0) (const Unmarked) xs

crossN :: Int -> Expr -> Expr
crossN i = foldr (.) id (replicate i Cross)

parseExpr' :: (Int, Int) -> (Expr -> Expr) -> String -> Expr
parseExpr' (0, 0) e ""  = (e Unmarked)
parseExpr' (i, j) e ('>':xs) = undefined
parseExpr' (i, j) e ('<':xs) = undefined

unmarked = Unmarked
marked = Cross Unmarked

-- | Equivalence

instance Eq Expr where
    (Call e1 e2) == (Call e1' e2') = e1 == e1' && e2 == e2'
    (Cross e) == (Cross e') = e == e'
    Unmarked == Unmarked = True
    Unmarked == _ = False
    _ == Unmarked = False

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

