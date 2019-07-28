{-# LANGUAGE OverloadedStrings #-}

module CalculusAsLogic where

import Reflex (Dynamic)
import Reflex.Dom
import qualified Data.Text as T

import Markup (parseAndRenderWidget)

calculusAsLogic :: (DomBuilder t m, PostBuild t m) => m ()
calculusAsLogic = do
  el "h1" $ text "The caclulus as logic"
  el "h2" $ text "Not a"
  parseAndRenderWidget "<a>"
  el "h2" $ text "a or b"
  parseAndRenderWidget "ab"
  el "h2" $ text "a and b"
  parseAndRenderWidget "<<a><b>>"
  el "h2" $ text "a implies b"
  parseAndRenderWidget "<a>b"
