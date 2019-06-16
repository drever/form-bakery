{-# LANGUAGE DeriveGeneric #-}

module Common where

import Reflex.Dom
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Hashable
import GHC.Generics
import qualified Data.Set as Set


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
  | Var Char deriving (Generic, Eq, Ord)

instance Hashable Expr where

type Env = Map.Map Char Expr

instance Show Expr where
  show (Cross e) = intercalate "" ["<", show e, ">"]
  show (Call es) = intercalate "" $ map show es
  show (Var a) = a:""

instance Read Expr where
   readsPrec _ s = either (error . show)
                          (\es -> [(call es, "")]) $
                          runParser pes () "" s
   readList s = either (error . show)
                       (\x -> [(x, "")]) $
                       runParser pes () "" s

call :: [Expr] -> Expr
call (x:[]) = x
call xs = Call xs

pe :: Parser Expr
pe = choice [pev, pe']

pe' :: Parser Expr
pe' = (Cross . call) <$> (char '<' *> pes <* char '>')

pev :: Parser Expr
pev = Var <$> letter

pes :: Parser [Expr]
pes = many pe

parseExpr :: String -> Either ParseError Expr
parseExpr s = Call <$> runParser pes () "" s

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

d = Map.fromList [('a', marked), ('b', marked)]

eval :: Env -> Expr -> Maybe Expr
eval _ (Call []) = Just unmarked
eval _ (Cross (Call [])) = Just marked
eval _ (Cross (Cross x)) = eval d x
eval d (Cross (Var x)) = eval d =<< Cross <$> Map.lookup x d
eval d (Var x) = Map.lookup x d
eval d (Call (e:[])) = eval d e
eval d (Call es) = case sequence $ map (eval d) es of
                      Nothing -> Nothing
                      (Just es') -> eval d $ Call $ (filter (/=unmarked)) $(Set.toList (Set.fromList es'))

-- | Appendix 2
-- | The calculus interpreted as logic

