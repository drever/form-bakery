{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom
import Data.FileEmbed
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text as T
import Index (indexOfForms)
import Introduction (introduction)
import CalculusAsLogic (calculusAsLogic)

main :: IO ()
main = mainWidgetWithCss css  $ do
  heading
  introduction
  calculusAsLogic

  el "p" $ text "What follows is the index of forms"
  indexOfForms
  -- el "p" (parseAndRenderWidget "<a>b")

  -- el "hr" (text "")
  -- display =<< holdDyn "bla" --(42, 0)
  --         . traceEvent "Position"
  --         -- . leftmost
  --         . mergeWith (++)
  --         . (fmap (fmap show))
  --         -- . mergeWith (\e1@(d1, _) e2@(d2, _) -> if d1 >= d2 then e1 else e2)
  --         =<< (either parseError expression . parseExpr $ "<<a>><>")
  -- -- case parseExpr "<<><>>" of
  -- --     Right e -> expressionSVG e
  --     -- Left err -> (el "p" $ text "error") >> return never
  return ()
   where css = $(embedFile "css/mark.css")

heading :: DomBuilder t m => m ()
heading = do
  el "h1" $ text "The Form Bakery"
  el "p" $ text "An invitation to the Laws of Form"

