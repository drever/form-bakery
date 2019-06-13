module Common where

import Reflex.Dom
import Data.List
import Data.Maybe

-- | 1
-- | The form

data Distinction = PerfectContinence

-- | 2
-- | Froms taken out of the form

-- | Expression

data Expr =
    Cross Expr
  | Call [Expr] --deriving (Show)

instance Show Expr where
--   show Unmarked = ""
  show (Cross e) = intercalate "" ["<", show e, ">"]
  show (Call es) = intercalate "" $ map show es

instance Read Expr where
   readsPrec _ = undefined

-- "<>" -> Cross Unmarked
-- "<><>" -> Call (Cross Unmarked) (Cross Unmarked)
-- "<<>>" -> Cross (Cross Unmarked)
-- "<><><>" -> Call (Cross Unmarked) (Call (Cross Unmarked) (Cross Unmarked))
-- "<<><>>" -> Cross (Call (Cross Unmarked) (Cross Unmarked))
-- "<<>><>" -> Call (Cross (Cross Unmarked)) (Cross Unmarked)

parseExpr' :: String -> (Maybe Expr, String)
parseExpr' ('<':xs) = case parseList xs of
                       (ps, '>':rs) -> (Just . Cross . Call $ ps, rs)
                       (_, rs) -> (Nothing, rs)
parseExpr' xs = (Nothing, xs)

parseExpr :: String -> (Maybe Expr, String)
parseExpr xs = case parseList xs of
                 ([], rs) -> (Nothing, rs)
                 (es, rs) -> (Just $ Call es, rs)

parseList :: String -> ([Expr], String)
parseList xs = case parseExpr' xs of
                 (Just x, rs) -> let (ys, qs) = parseList rs
                                  in (x:ys, qs)
                 (Nothing, rs) -> ([], rs)


-- parseExpr' ss "" = callN ss
-- parseExpr' ss ('<':xs) = parseExpr' (Unmarked : ss) xs
-- parseExpr' (s:ss) ('>':[]) = callN (Cross s : ss)
-- parseExpr' (s:ss) ('>':'>':xs) = parseExpr' (Cross s:ss) ('>':xs)
-- parseExpr' (s:ss) ('>':'<':xs) = Call (Cross s) (parseExpr' ss ('<':xs))

tt p = (show . parseExpr $ p) == p
unmarked = Call []
marked = Cross unmarked

-- | Equivalence

-- instance Eq Expr where
--     -- (Call es) == (Call e1' e2') = e1 == e1' && e2 == e2'
    -- (Cross e) == (Cross e') = e == e'
    -- Unmarked == Unmarked = True
    -- Unmarked == _ = False
    -- _ == Unmarked = False

-- | Primitive equation

-- primitive1 (Call (Cross Unmarked) (Cross Unmarked)) = Call (Cross Unmarked) Unmarked
-- primitive1 e = e

-- primitive2 (Cross (Cross Unmarked)) = Unmarked
-- primitive2 e = e

-- | Simple expressions

simpleExpressions = [
        Call [marked, marked]
      , Cross marked
      , Cross unmarked
      , unmarked]

-- | Operation
cross = Cross

-- | Relation

continence :: Expr -> Expr -> Bool
continence = undefined

-- | 3
-- | The conception of calculation

-- | Fourth canon. Hypothesis of simplification

-- fourthCanon :: Expr
-- fourthCanon = Call [Cross (Call [Cross Unmarked, Cross Unmarked]), Cross Unmarked]
-- fourthCanon' = Call [Cross (Call marked marked), marked]

theorem3 = fromJust $ fst $ parseExpr "<<><>><>"
-- theorem3 =
--   Call [Cross
--      (Call [Cross
--          (Call
--                (Cross (Call [marked, marked]))
--                (marked)))
--         (Cross marked)))
--     (Cross (Cross (Cross marked))]]

data AlgExpr = AlgExpr

-- | Appendix 2
-- | The calculus interpreted as logic

