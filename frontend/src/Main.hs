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
  return ()

   where css = $(embedFile "css/mark.css")

heading :: DomBuilder t m => m ()
heading = do
  el "h1" $ text "Form Bakery"
  el "p" $ text "An invitation to the Laws of Form"

parseAndRenderWidget :: (DomBuilder t m, PostBuild t m) => m ()
parseAndRenderWidget = do
      t <-  inputElement $ def
           & inputElementConfig_initialValue .~ "<a>b"
      dyn $ either parseError expression
            . parseExpr
            <$> _inputElement_value t
      let env = Map.fromList [('a', marked), ('b', marked)]
      dyn $ either parseError expression
           . (\t' -> parseExpr t' >>= eval env)
           <$> _inputElement_value t
      return ()

