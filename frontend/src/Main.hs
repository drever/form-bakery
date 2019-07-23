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
      p <-  expression e
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

xxmain :: IO ()
xxmain = mainWidget bla

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  el "h2" $ text "Text Input - Configuration"

  el "h4" $ text "Max Length 14"
  t1 <- textInput $ def & attributes .~ constDyn ("maxlength" =: "14")
  dynText $ _textInput_value t1

  el "h4" $ text "Initial Value"
  t2 <- textInput $ def & textInputConfig_initialValue .~ "input"
  dynText $ _textInput_value t2

  el "h4" $ text "Input Hint"
  t3 <- textInput $
        def & attributes .~ constDyn("placeholder" =: "type something")
  dynText $ _textInput_value t3

  el "h4" $ text "Password"
  t4 <- textInput $ def & textInputConfig_inputType .~ "password"
  dynText $ _textInput_value t4

  el "h4" $ text "Multiple Attributes: Hint + Max Length"
  t5 <- textInput $  def & attributes .~ constDyn ("placeholder" =: "Max 6 chars" <> "maxlength" =: "6")
  dynText $ _textInput_value t5

  el "h4" $ text "Numeric Field with initial value"
  t6 <- textInput $ def & textInputConfig_inputType .~ "number"
                        & textInputConfig_initialValue .~ "0"
  dynText $ _textInput_value t6

  return ()

bodyElementHoldDyn :: MonadWidget t m => m ()
bodyElementHoldDyn = do
  el "h2" $ text "Text Input - Read Value on Button Click"
  ti <- textInput def
  evClick <- button "Click Me"
  el "br" blank
  text "Contents of TextInput on last click: "
  let evText = tagPromptlyDyn (value ti) evClick
  dynText =<< holdDyn "" evText

bodyElementKeypress :: MonadWidget t m => m ()
bodyElementKeypress = do
  el "h2" $ text "Text Input - Read Value on 'Enter'"
  ti <- textInput def
  el "br" blank
  text "Contents of TextInput after 'Enter': "
  let evEnter = keypress Enter ti
  let evText = tagPromptlyDyn (value ti) evEnter
  dynText =<< holdDyn "" evText

bodyWriteIntoTextInput :: MonadWidget t m => m ()
bodyWriteIntoTextInput = do
  el "h1" $ text "Write into TextInput Widget"
  t1 <- textInput def
  evCopy <- button ">>>"
  let evText = tagPromptlyDyn (value t1) evCopy
  t2 <- textInput $ def & setValue .~ evText
  return ()

bla :: MonadWidget t m => m ()
bla = do
  rec ti <- textInput $ def
                        & textInputConfig_initialValue .~ "<>"
                        & setValue .~ ft
      ft <- twoButtons (value ti)
  return ()

twoButtons :: (PostBuild t m, DomBuilder t m) => Dynamic t T.Text -> m (Event t T.Text)
twoButtons dt = do
  let dt1 = (\x -> (x <> x)) <$> dt
      dt2 = T.reverse <$> dt
  b1 <- do (e, _) <- element "button" def $ dynText dt1
           return $ domEvent Click e
  b2 <- do (e, _) <- element "button" def $ dynText dt2
           return $ domEvent Click e
  let evText1 = tagPromptlyDyn dt1 b1
  let evText2 = tagPromptlyDyn dt2 b2
  return $ evText1 <> evText2


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

