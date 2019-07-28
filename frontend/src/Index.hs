{-# LANGUAGE OverloadedStrings #-}

module Index where

import Reflex (Dynamic)
import Reflex.Dom
import qualified Data.Text as T

import Markup (parseAndRenderWidget, consequence)

indexOfForms :: (DomBuilder t m, PostBuild t m) => m ()
indexOfForms = do
  el "h1" $ text "Index of forms"
  el "h2" $ text "Consequences"

  el "h3" $ text "C1"
  consequence "<<a>>" "a"
  el "h3" $ text "C2"
  consequence "<ab>b" "<a>b"
  el "h3" $ text "C3"
  consequence "<>a" "<>"
  el "h3" $ text "C4"
  consequence "<<a>b>a" "a"
  el "h3" $ text "C5"
  consequence "aa" "a"
  el "h3" $ text "C6"
  consequence "<<a><b>><<a>b>" "a"
  el "h3" $ text "C7"
  consequence "<<<a>b>c>" "<ac><<b>c>"
  el "h3" $ text "C8"
  consequence "<<a><br><cr>>" "<<a><b><c>><<a><r>>"
  el "h3" $ text "C9"
  consequence "<<<b><r>><<a><r>><<x>r><<y>r>>" "<<r>ab><rxy>"

