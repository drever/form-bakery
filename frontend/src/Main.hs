{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom

import qualified Data.Text as T
import Common

main :: IO ()
main = mainWidget $ do
  el "p" $ text "Hello, Spencer Brown!"
  text (T.pack . show $ theorem3)
