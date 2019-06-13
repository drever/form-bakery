{-# LANGUAGE OverloadedStrings #-}

module Markup where

import Common
import Reflex.Dom

showM (Call []) = elClass "div" "unmarked" (text "")
showM (Call es) = elClass "div" "call" $ (mapM_ showM es)
showM (Cross e) = elClass "div" "cross" (showM e)
