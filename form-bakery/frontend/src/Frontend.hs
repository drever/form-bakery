{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Reflex.Dom.Core

import Common.Api
import Common.Route
import Index (indexOfForms)
import Introduction (heading, introduction, primaryAlgebra, primaryArithmetic, about)
import CalculusAsLogic (calculusAsLogic)
import Control.Lens ((^?), (+~), (?~), (#), from, at)

import Markup (parseError, expression, parseAndRenderWidget, expressionWidget, theRangeTester, dragTester)

import Obelisk.Generated.Static

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Form bakery"
  , _frontend_body = do
      elAttr "link" ("rel" =: "stylesheet"
                & at "href" ?~ static @"mark.css") blank

      -- theRangeTester
      -- parseAndRenderWidget "<<>><a><>"
      dragTester

      elAttr "img" ("src" =: static @"obelisk.jpg") blank
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s
      return ()
  }
