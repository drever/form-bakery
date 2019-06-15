module Common where

import Reflex.Dom
import Data.List
import Data.Maybe

import Text.ParserCombinators.Parsec

-- | 1
-- | The form

data Distinction = PerfectContinence

-- | 2
-- | Froms taken out of the form

-- | Expression

data Expr =
    Cross Expr
  | Call [Expr]
  | Var Char

instance Show Expr where
  show (Cross e) = intercalate "" ["<", show e, ">"]
  show (Call es) = intercalate "" $ map show es
  show (Var a) = a:[]

instance Read Expr where
   readsPrec _ s = either (error . show)
                          (\es -> [(Call es, "")]) $
                          runParser pes () "" s
   readList s = either (error . show)
                       (\x -> [(x, "")]) $
                       runParser pes () "" s

pe :: Parser Expr
pe = choice [pev, pe']

pe' :: Parser Expr
pe' = (Cross . Call) <$> (char '<' *> pes <* char '>')

pev :: Parser Expr
pev = Var <$> letter

pes :: Parser [Expr]
pes = many pe

parseExpr = runParser pe () ""

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

theorem3 :: Expr
theorem3 = read "<<><>><>"

data AlgExpr = AlgExpr

-- | Appendix 2
-- | The calculus interpreted as logic

