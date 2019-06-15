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
  | Call [Expr]

instance Show Expr where
  show (Cross e) = intercalate "" ["<", show e, ">"]
  show (Call es) = intercalate "" $ map show es

instance Read Expr where
   readsPrec _ s = case parseExpr s of
                      (Nothing, _) -> error $ "no parse: " ++ s
                      (Just x, r) -> [(x, r)]
   readList s = return $ parseList s

parseExpr :: String -> (Maybe Expr, String)
parseExpr "" = (Just unmarked, "")
parseExpr xs = case parseList xs of
                 ([], rs) -> (Nothing, rs)
                 (es, rs) -> (Just $ Call es, rs)

parseExpr' :: String -> (Maybe Expr, String)
parseExpr' ('<':xs) = case parseList xs of
                       (ps, '>':rs) -> (Just . Cross . Call $ ps, rs)
                       (_, rs) -> (Nothing, rs)
parseExpr' xs = (Nothing, xs)

parseList :: String -> ([Expr], String)
parseList xs = case parseExpr' xs of
                 (Just x, rs) -> let (ys, qs) = parseList rs
                                  in (x:ys, qs)
                 (Nothing, rs) -> ([], rs)

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

fourthCanon :: Expr
fourthCanon = Call [Cross (Call [Cross unmarked, Cross unmarked]), Cross unmarked]
fourthCanon' = Call [Cross (Call [marked, marked]), marked]

theorem3 = fromJust $ fst $ parseExpr "<<><>><>"

data AlgExpr = AlgExpr

-- | Appendix 2
-- | The calculus interpreted as logic

