{-# LANGUAGE OverloadedStrings #-}

module CalculusAsLogic where

import Reflex (Dynamic)
import Reflex.Dom
import qualified Data.Text as T
import Control.Lens ((^?), (+~), (?~), (#), from, at)

import Markup (parseAndRenderWidget)


calculusAsLogic :: (DomBuilder t m, PostBuild t m) => m ()
calculusAsLogic = do
  elAttr "h1" (mempty & at "id" ?~ "calculus-as-logic") $ text "The caclulus as logic"

  el "p" $ do text "The Laws of Form can be used to represent Boolean logic. Here are some examples of Boolean functions represented in the calculus of indications."
              el "ul" $ do
                el "li" $ elAttr' "a" (mempty & at "href" ?~ "#boolean-not") $ text "Not"
                el "li" $ elAttr' "a" (mempty & at "href" ?~ "#boolean-or") $ text "Or"
                el "li" $ elAttr' "a" (mempty & at "href" ?~ "#boolean-and") $ text "And"
                el "li" $ elAttr' "a" (mempty & at "href" ?~ "#boolean-implies") $ text "Implies"
  elAttr "h3" (mempty & at "id" ?~ "boolean-not") $ text "Not a"
  parseAndRenderWidget "<a>"
  elAttr "h3" (mempty & at "id" ?~ "boolean-or") $ text "a or b"
  parseAndRenderWidget "ab"
  elAttr "h3" (mempty & at "id" ?~ "boolean-and") $ text "a and b"
  parseAndRenderWidget "<<a><b>>"
  elAttr "h3" (mempty & at "id" ?~ "boolean-implies") $ text "a implies b"
  parseAndRenderWidget "<a>b"
