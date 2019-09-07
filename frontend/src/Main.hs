{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Reflex.Dom
import Data.FileEmbed
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text as T
import Common
import Markup
import Control.Monad.Fix
import Control.Lens ((%~), (^?), (+~), (?~), (#), from, at)
import Data.Proxy (Proxy (..))


main :: IO ()
main = mainWidgetWithCss css  $ do
  -- heading
  -- el "p" (parseAndRenderWidget "<a>b")



  -- el "hr" (text "")
  -- display =<< holdDyn ""
  -- rec e <- foldDyn insertMarkAt initialExpr p
  --     p <- expression e
          -- . traceEvent "Position"
          -- . leftmost
          -- . mergeWith (const)
          -- . (fmap T.unpack)
          -- . mergeWith (\e1@(d1, _) e2@(d2, _) -> if d1 >= d2 then e1 else e2)
              -- =<< (either parseError expression . parseExpr $ "<<a>><>")
  -- case parseExpr "<<><>>" of
  --     Right e -> expressionSVG e
      -- Left err -> (el "p" $ text "error") >> return never


  rg <- rangeInput (def &
                 rangeInputConfig_initialValue .~ 16)
  let xv = floor <$> _rangeInput_value rg
  display xv


  e3 <- dyn $ div1 <$> (+1) <$> xv

  dynText $ (T.pack . show) <$> xv
  e3' <- switchHoldPromptly (never ) e3
  dynText =<< foldDyn (\a _ -> (T.pack . show $ a)) "let's go!" (e3')

  return ()

   where css = $(embedFile "css/mark.css")
         initialExpr = let (Right e) = parseExpr "<<a>><>"
                        in e :: Expr



c0 :: (DomBuilder t m, PostBuild t m) => Dynamic t T.Text -> m (Event t T.Text)
c0 dynTxt = do postBuild <- getPostBuild
               (t, _) <- elAttr' "div" def $
                      textNode $ def
                            & textNodeConfig_setContents .~ leftmost [
                                (updated dynTxt)
                              , tag (current dynTxt) postBuild
                              ]
               notReadyUntil postBuild
               return $ const "Hallo Welt" <$> domEvent Click t

div1 :: forall m t. (DomBuilder t m, PostBuild t m) => Int -> m (Event t [Int])
div1 i = if i > 1
    then do -- (t, _) <- elAttr' "div" (st i) (div1 (i `div` 2))
            let content = (div1 (i `div` 2))
            (t, es) <- element
                          "div"
                          (elementConfig i)
                          content
            return $ -- (traceEvent $ "hey" <> (show i) <> ": ") $
                 let newEvent = const [i] <$> domEvent Click t
                 in es <> newEvent

    else do (t, _) <- element "div" (elementConfig i) blank
            return $ -- (traceEvent $ "hey" <> (show i) <> ": ") $
                const [i] <$> domEvent Click t

      where colrs =
             concat $ repeat ["#FFEB3B", "#2196F3", "#FF00FF", "#04F3A5", "#60FBAA"]
            st :: Int -> Map.Map T.Text T.Text
            st i = (mempty & at "style" ?~ ("width: " <> (T.pack . show $ i * 10) <> "px"
                                     <> "; height: " <> (T.pack . show $ i * 10) <> "px"
                                     <> "; background: " <> (colrs !! (i - 1))
                                     ))
            elementConfig' :: Int -> ElementConfig EventResult t (DomBuilderSpace m)
            elementConfig' i = def & (initialAttributes .~ Map.mapKeys (AttributeName Nothing) (st i))
            elementConfig i =
                          (elementConfig' i)
                               { _elementConfig_eventSpec =
                                      (addEventSpecFlags
                                           (Proxy :: Proxy (DomBuilderSpace m))
                                           Click
                                           (const stopPropagation)
                                           (_elementConfig_eventSpec (elementConfig' i))) }

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

