{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom
import Data.FileEmbed
import Data.Maybe

import qualified Data.Text as T
import Common
import Markup

main :: IO ()
main = mainWidgetWithCss css  $ do
  heading
  el "div" parseAndRenderWidget
--  display =<< count =<< exprElement "<<>><>"
  return ()

   where css = $(embedFile "css/mark.css")

heading :: DomBuilder t m => m ()
heading =
  el "h1" $ text "An invitation to the Laws of Form"

parseAndRenderWidget :: (DomBuilder t m, PostBuild t m) => m ()
parseAndRenderWidget = do
      t <-  inputElement $ def
           & inputElementConfig_initialValue .~ (T.pack . show $ theorem3)
      dyn $ exprElement <$> _inputElement_value t
--      dynText $ (T.pack . show . (read :: String -> Expr) . T.unpack) <$> _inputElement_value t
      return ()


