{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex.Dom
import Data.FileEmbed
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text as T
import Common
import Markup
import Control.Monad.Fix

main :: IO ()
main = mainWidgetWithCss css  $ do
  heading
  -- el "p" (parseAndRenderWidget "<a>b")



  el "hr" (text "")
  -- display =<< holdDyn ""
  rec e <- foldDyn insertMarkAt initialExpr p
      p <- expression e
          -- . traceEvent "Position"
          -- . leftmost
          -- . mergeWith (const)
          -- . (fmap T.unpack)
          -- . mergeWith (\e1@(d1, _) e2@(d2, _) -> if d1 >= d2 then e1 else e2)
              -- =<< (either parseError expression . parseExpr $ "<<a>><>")
  -- case parseExpr "<<><>>" of
  --     Right e -> expressionSVG e
      -- Left err -> (el "p" $ text "error") >> return never
  return ()

   where css = $(embedFile "css/mark.css")
         initialExpr = let (Right e) = parseExpr "<<a>><>"
                        in e :: Expr

heading :: DomBuilder t m => m ()
heading = do
  el "h1" $ text "The Form Bakery"
  el "p" $ text "An invitation to the Laws of Form"

-- parseAndRenderWidget :: (DomBuilder t m, PostBuild t m) => T.Text -> m ()
-- parseAndRenderWidget e = do
--       t <-  inputElement $ def
--            & inputElementConfig_initialValue .~ e
--       elClass "div" "output" $ do
--         dyn $ updated <$> either parseError expression
--                . parseExpr
--                <$> _inputElement_value t
--           dyn $ either (const (return never)) truthTable
--               . parseExpr
--               <$> _inputElement_value t
--       return ()

