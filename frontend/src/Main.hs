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
main = mainWidgetWithCss css  $ el "div" $ do
  t <-  inputElement $ def
       & inputElementConfig_initialValue .~ (T.pack . show $ theorem3)
  dyn $ (fmap ((\x -> case x of
                        Just x' -> showM x'
                        Nothing -> text "???") . fst . parseExpr . T.unpack)) $ _inputElement_value t

  el "p" $ text "An invitation to the Laws of Form"
   where css = $(embedFile "css/mark.css")
