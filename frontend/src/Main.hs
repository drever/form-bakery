{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom
import Data.FileEmbed
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text as T
import Common
import Markup

main :: IO ()
main = mainWidgetWithCss css  $ do
  heading
  el "div" parseAndRenderWidget
  el "hr" (text "")
  display =<< count =<< (either parseError expression . parseExpr $ "<<>><>")
  case parseExpr "<<><>>" of
      Right x -> return expressionSVG x
      Left err -> (el "p" $ text "error") >> return never
  return ()

   where css = $(embedFile "css/mark.css")

heading :: DomBuilder t m => m ()
heading = do
  el "h1" $ text "The Form Bakery"
  el "p" $ text "An invitation to the Laws of Form"

parseAndRenderWidget :: (DomBuilder t m, PostBuild t m) => m ()
parseAndRenderWidget = do
      t <-  inputElement $ def
           & inputElementConfig_initialValue .~ "<a>b"
      elClass "div" "output" $ do
          dyn $ either parseError expression
               . parseExpr
               <$> _inputElement_value t

          dyn $ either (const (return never)) truthTable
              . parseExpr
              <$> _inputElement_value t
      return ()

