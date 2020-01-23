{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

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

import Markup (parseError, expression, parseAndRenderWidget, expressionWidget, theRangeTester, dragTester)
import Common (parseExpr, insertMarkAt, Position, Expr, marked)
import Control.Monad.Fix

import Data.Proxy

main :: IO ()
main = let e = "<<a>b>"
        in mainWidgetWithCss css $ do

              theRangeTester
              parseAndRenderWidget "<<>><a><>"
              dragTester

              --        & inputElementConfig_initialValue .~ e

              -- e <- either (\e -> parseError e >> updated $ return never) expressionWidget
              --          . parseExpr
              --          <$> _inputElement_value t

              -- heading
              -- introduction
              -- primaryArithmetic
              -- primaryAlgebra
              -- calculusAsLogic
              -- indexOfForms
              -- about

              return ()

   where css = $(embedFile "css/mark.css")

