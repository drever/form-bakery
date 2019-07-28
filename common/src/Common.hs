{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Common where

import Reflex.Dom
import Data.List
import Data.Maybe
import Data.Either
import qualified Data.Map as Map
import Data.Hashable
import GHC.Generics
import Data.List
import qualified Data.Text as T
import Control.Monad
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
type Position = T.Text

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

listValues :: Expr -> [(Env, Expr)]
listValues e = map (\env -> let (Right r) = eval env e
                             in (env, r)) (allEnvs e)

insertMarkAt :: Expr -> Position -> Expr
insertMarkAt e "B" = Cross e
insertMarkAt um@(Call []) "" = Cross um
insertMarkAt m@(Cross _) (T.uncons -> Just ('0', rs)) = insertMarkAt m rs
insertMarkAt (Cross e) (T.uncons -> Just ('C', rs)) = Cross $ insertMarkAt e rs
insertMarkAt (Call es) (T.uncons -> Just (r, rs)) =
    let i = read (r:[])
    in Call $ foldr (\(j, e) acc ->
           if i == j
                then (insertMarkAt e rs):acc
                else e:acc) [] (zip [0..] es)

allEnvs :: Expr -> [Env]
allEnvs e = let getVars :: Expr -> [Char]
                getVars (Cross e) = getVars e
                getVars (Call es) = concat $ map getVars es
                getVars (Var v) = return v

                vs = sort . Set.toList . Set.fromList . getVars $ e
                bs = replicateM (length vs) [marked, unmarked]
             in map (Map.fromList . (zip vs)) bs

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

parseExpr :: T.Text -> Either String Expr
parseExpr s = either (Left . show) (Right . Call . id) $
                 runParser pes () "" (T.unpack s)

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

eval :: Env -> Expr -> Either String Expr
eval _ (Call []) = Right unmarked
eval d (Call es) = case mapM (eval d) es of
                      Left err -> Left err
                      (Right es') -> Right $ case filter (==marked) $ es' of
                                       [] ->unmarked
                                       _ -> marked
eval d (Var x) = case Map.lookup x d of
                    Nothing -> Left $ "Variable not in scope: " ++ x:""
                    (Just v) -> eval d v
eval d (Cross v) = case eval d v of
                     Left err -> Left err
                     (Right (Call [])) -> Right marked
                     (Right (Call es)) -> Right unmarked
                     (Right (Cross x)) -> Right x

-- apply :: Expr -> Position -> Expr -> Expr
-- apply

data Laws = Laws (Expr -> Expr)

laws = Laws $ \case
     -- I1
     (Call [(Cross (Call [])), (Cross (Call []))]) ->  marked
     -- I2
     (Cross (Cross (Call []))) -> unmarked
     -- J1
     -- <<p>p> =
     e@(Cross (Call [Cross (Var p), Var p'])) ->
       if  p == p'
          then unmarked
          else e
     -- J2
     e@(Cross (Call [Cross (Call [Var p, Var r]), Cross (Call [Var q, Var r'])])) ->
       if r == r'
          then Call [Cross (Call [Cross (Var p), Cross (Var q)]), Var r]
          else e




