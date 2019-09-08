{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom
import Data.FileEmbed
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text as T
import Index (indexOfForms)
import Introduction (heading, introduction, primaryAlgebra, primaryArithmetic, about)
import CalculusAsLogic (calculusAsLogic)
import Control.Lens ((^?), (+~), (?~), (#), from, at)

import Markup (parseError, expression, parseAndRenderWidget)
import Common (parseExpr)

main :: IO ()
main = mainWidgetWithCss css  $ do


  heading
  introduction
  primaryArithmetic
  primaryAlgebra
  calculusAsLogic
  indexOfForms
  about

  -- el "p" $ do let pwidget = (either (\x -> do parseError x
  --                                             return never) expression) . parseExpr $ "<<a>b>"
  --             e <- pwidget
  --             display =<< (holdDyn ["Los geht's!"] e)
  --             return e

  return ()
   where css = $(embedFile "css/mark.css")

