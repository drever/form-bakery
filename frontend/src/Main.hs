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

-- import qualified Language.Javascrip.JSaddle as DOM

-- foreign import javascript unsafe
--     "$($1).sidebar({  dimPage: $2   });alert($2);"
    -- js_configSidebar :: Element -> Bool -> DOM.JSM ()

main :: IO ()
main = let e = "<<a>b>"
        in mainWidgetWithCss css $ do
              (t, _) <- el' "div" $ blank

              -- theRangeTester
              parseAndRenderWidget "a"
              -- mapM_ (dragTester . T.pack . show) [1..1]

              -- xs <- foldDyn const (42.0) (domEvent Scroll t)
              -- display xs

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

