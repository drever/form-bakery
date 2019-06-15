{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Markup (exprElement) where

import Common
import Reflex.Dom
import Control.Monad.State

import qualified Data.Text as T

exprElement :: DomBuilder t m => T.Text -> m ()
exprElement = match . parseExpr . T.unpack
    where match = \case
            Right x' -> showM x'
            Left err -> text (T.pack $ show err) -- >> (return never)

showM :: DomBuilder t m
      => Expr
      -> m ()
showM = snd . (showM' (0, 0))

showM' :: DomBuilder t m
          => (Int, Int)
          -> Expr
          -> ((Int, Int), m ())
showM' (i, j) (Call []) = (
            (i, j)
          , divButton
                "unmarked" (T.pack $ show (i, j))
                (text ""))

showM' (i, j) (Var e) = (
              (i, j)
           , divButton
                "var" (T.pack $ show (i, j))
                (text (T.pack $ e:[])))

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

divButton :: DomBuilder t m
          => T.Text
          -> T.Text
          -> m a
          -> m ()
divButton c cs e = do
           (e', _) <- elClass' "div" c e
--           return $ domEvent Click e'
           return ()

