{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Markup (expression
             , parseError
             , truthTable) where

import Common
import Reflex.Dom
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T

parseError err = (text . T.pack . show $ err) >> (return never)

truthTable :: DomBuilder t m
      => Expr
      -> m (Event t ())
truthTable e = case listValues e of
                 [] -> (text "") >>  return never
                 vs -> do el "table" $ do
                             el "tr" $ do let ns = map fst . Map.toList . fst . head $ vs :: [Char]
                                          mapM_ (\v -> el "th" (text (T.pack . (:[]) $ v))) ns
                                          el "th" (expression e)
                             mapM_ (\vs'' -> el "tr" $ do let vs' = map snd . Map.toList . fst $ vs'' :: [Expr]
                                                          mapM_ (\v -> el "td" (expression $ v)) (vs' :: [Expr])
                                                          el "td" (expression . snd $ vs'')) (vs :: [(Env, Expr)])
                          return never

expression :: DomBuilder t m
      => Expr
      -> m (Event t ())
expression = snd . (expression' (0, 0))

expression' :: DomBuilder t m
          => (Int, Int)
          -> Expr
          -> ((Int, Int), m (Event t ()))
expression' (i, j) (Call []) = (
            (i, j)
          , divButton
                "unmarked" (T.pack $ show (i, j))
                (text ""))

expression' (i, j) (Var e) = (
              (i, j)
           , divButton
                "var" (T.pack $ show (i, j))
                (text (T.pack $ e:[])))

expression' (i, j) (Call es) = (
            (i, j)
          , divButton
                "call" (T.pack $ show (i, j))
                (mapM_ ((\(j', e) -> snd $ expression' (i, j') e)) (zip [1..] es)))

expression' (i, j) (Cross e) = (
            (i, j)
          , divButton
                "cross" (T.pack $ show (i, j))
                (snd $ expression' (i + 1, j) e))

divButton :: DomBuilder t m
          => T.Text
          -> T.Text
          -> m a
          -> m (Event t ())
divButton c cs e = do
          (e', _) <- elClass' "div" c e
          return $ domEvent Click e'

