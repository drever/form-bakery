{-# LANGUAGE OverloadedStrings #-}

module Index where

import Reflex (Dynamic)
import Reflex.Dom
import qualified Data.Text as T

import Markup (parseAndRenderWidget, consequence, highlight)
import Control.Lens ((^?), (+~), (?~), (#), from, at)
import Control.Monad.Fix

indexOfForms :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
indexOfForms = do
  elAttr "h1" (mempty & at "id" ?~ "index-of-forms") $ text "Index of forms"
  el "p" $ text "The index of forms lists all theorems and consecquences of the Laws of Form. The nine consequences of the primary arithmetic are listed here. Use this as a reference."

  el "ul" $ mapM (\(i, (e1, e2)) ->
    el "li" $ (elAttr "a" (mempty & at "href" ?~ "#consequence-" <> (T.pack . show $ i)) $ text ("C" <> (T.pack . show $ i))) >> highlight (e1 <> " = " <> e2)) (zip [1..9]
      [ ("<<a>>", "a")
      , ("<ab>b", "<a>b")
      , ("<>a", "<>")
      , ("<<a>b>a", "a")
      , ("aa", "a")
      , ("<<a><b>><<a>b>", "a")
      , ("<<<a>b>c>", "<ac><<b>c>")
      , ("<<a><br><cr>>", "<<a><b><c>><<a><r>>")
      , ("<<<b><r>><<a><r>><<x>r><<y>r>>", "<<r>ab><rxy>")])

  elAttr "h3" (mempty & at "id" ?~ "consequence-1") $ text "C1"
  consequence "<<a>>" "a"
  elAttr "h3" (mempty & at "id" ?~ "consequence-2") $ text "C2"
  consequence "<ab>b" "<a>b"
  elAttr "h3" (mempty & at "id" ?~ "consequence-3") $ text "C3"
  consequence "<>a" "<>"
  elAttr "h3" (mempty & at "id" ?~ "consequence-4") $ text "C4"
  consequence "<<a>b>a" "a"
  elAttr "h3" (mempty & at "id" ?~ "consequence-5") $ text "C5"
  consequence "aa" "a"
  elAttr "h3" (mempty & at "id" ?~ "consequence-6") $ text "C6"
  consequence "<<a><b>><<a>b>" "a"
  elAttr "h3" (mempty & at "id" ?~ "consequence-7") $ text "C7"
  consequence "<<<a>b>c>" "<ac><<b>c>"
  elAttr "h3" (mempty & at "id" ?~ "consequence-8") $ text "C8"
  consequence "<<a><br><cr>>" "<<a><b><c>><<a><r>>"
  elAttr "h3" (mempty & at "id" ?~ "consequence-9") $ text "C9"
  consequence "<<<b><r>><<a><r>><<x>r><<y>r>>" "<<r>ab><rxy>"

