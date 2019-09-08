{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom
import Data.FileEmbed
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text as T
import Index (indexOfForms)
import Introduction (primaryAlgebra, primaryArithmetic)
import CalculusAsLogic (calculusAsLogic)
import Control.Lens ((^?), (+~), (?~), (#), from, at)

import Markup (parseError, expression, parseAndRenderWidget)
import Common (parseExpr)

main :: IO ()
main = mainWidgetWithCss css  $ do


  -- heading
  -- el "p" $ do text "This is a playground for "
  --             elAttr "a" (mempty & at "href" ?~ "https://en.wikipedia.org/wiki/Laws_of_Form") $ text "Laws of Form"
  --             text ". The contents of the books are presented in an interactive fashion, following the literal programming philosophy."
  --             el "ul" $ do
  --               el "li" $ elAttr "a" (mempty & at "href" ?~ "#primary-arithmetic") $ text "The primary arithmetic"
  --               el "li" $ elAttr "a" (mempty & at "href" ?~ "#primary-algebra") $ text "The primary algebra"
  --               el "li" $ elAttr "a" (mempty & at "href" ?~ "#calculus-as-logic") $ text "The calculus as logic"
  --               el "li" $ elAttr "a" (mempty & at "href" ?~ "#index-of-forms") $ text "The index of forms"
  --               el "li" $ elAttr "a" (mempty & at "href" ?~ "#about") $ text "About"
  -- primaryArithmetic
  -- primaryAlgebra
  -- calculusAsLogic
  -- indexOfForms
  -- about

  -- el "p" (parseAndRenderWidget "<a>b")

  el "p" $ do let pwidget = (either (\x -> do parseError x
                                              return never) expression) . parseExpr $ "<<a>b>"
              e <- pwidget
              display =<< (holdDyn ["Los geht's!"] e)
              return e


  -- el "hr" (text "")
  -- display =<< holdDyn "Los gehts!" --(42, 0)
  --         . traceEvent "Position"
  --         -- . leftmost
  --         -- . mergeWith (++)
  --         -- . (fmap (fmap show))
  --         -- . mergeWith (\e1@(d1, _) e2@(d2, _) -> if d1 >= d2 then e1 else e2)
  --         =<< (either (parseError >> return never) expression . parseExpr $ "<<a>><>")

  return ()
   where css = $(embedFile "css/mark.css")

heading :: DomBuilder t m => m ()
heading = do
  el "h1" $ text "Form Bakery - An invitation to Laws of Form"

about :: DomBuilder t m => m ()
about = do
  elAttr "h1" (mempty & at "id" ?~ "about") $ text "About"
  el "p" $ do text "Written in 2019 by Johannes Drever. The source code can be found on github: "
              elAttr "a" (mempty & at "href" ?~ "https://github.com/drever/form-bakery") $ text "https://github.com/drever/form-bakery"
              text ". The form bakery has been inspired by "
              elAttr "a" (mempty & at "href" ?~ "http://www.markability.net/") $ text "the markable mark"
              text "."

