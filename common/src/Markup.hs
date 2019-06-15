{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Markup (showM, exprElement) where

import Common
import Reflex.Dom

import qualified Data.Text as T

exprElement :: DomBuilder t m => T.Text -> m (Event t ())
exprElement = match . fst . parseExpr . T.unpack
    where match = \case
            Just x' -> showM x'
            Nothing -> text "???" >> (return never)

showM :: DomBuilder t m => Expr -> m (Event t ())
showM = snd . (showM' (0, 0))

showM' :: DomBuilder t m => (Int, Int) -> Expr -> ((Int, Int), m (Event t ()))
showM' (i, j) (Call []) = (
            (i, j)
          , divButton
                "unmarked" (T.pack $ show (i, j))
                (text ""))

showM' (i, j) (Call es) = (
            (i, j)
          , divButton
                "call" (T.pack $ show (i, j))
                (mapM_ ((\(j', e) -> snd $ showM' (i, j') e)) (zip [1..] es)))

showM' (i, j) (Cross e) = (
            (i, j)
          , divButton
                "cross" (T.pack $ show (i, j))
                (snd $ showM' (i + 1, j) e))

divButton :: DomBuilder t m => T.Text -> T.Text -> m a -> m (Event t ())
divButton c cs e = do
            (e', _) <- elClass' "div" c e
            return $ domEvent Click e'
