{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex.Dom
import Data.FileEmbed
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text as T
import Index (indexOfForms)
import Introduction (heading, introduction, primaryAlgebra, primaryArithmetic, about)
import CalculusAsLogic (calculusAsLogic)
import Control.Lens ((^?), (+~), (?~), (#), from, at)

import Markup (parseError, expression, parseAndRenderWidget)
import Common (parseExpr, insertMarkAt, Position, Expr)

import Data.Proxy

main :: IO ()
main = mainWidgetWithCss css  $ do
  rec

  -- heading
  -- introduction
  -- primaryArithmetic
  -- primaryAlgebra
  -- calculusAsLogic
  -- indexOfForms
  -- about

      let (Right expr) = parseExpr "<<a>b>"
      e <- expression expr :: HydrationDomBuilderT
                        GhcjsDomSpace
                        DomTimeline
                        (DomCoreWidget x)
                        (Event (SpiderTimeline Global) [Position])
      el "div" $ display =<< (holdDyn ["Los geht's!"] e)

      let newExpression = (insertMarkAt expr . head) <$> e :: Event (SpiderTimeline Global) Expr
      dynExpression <- foldDyn
                           (\a _ -> a)
                           expr
                           newExpression :: HydrationDomBuilderT
                                  GhcjsDomSpace
                                  DomTimeline
                                  (DomCoreWidget x)
                                  (Dynamic (SpiderTimeline Global) Expr)


      ee <- dyn (expression <$> dynExpression') :: HydrationDomBuilderT
                        GhcjsDomSpace
                        DomTimeline
                        (DomCoreWidget x)
                        (Event (SpiderTimeline Global) (Event (SpiderTimeline Global) [Position]))

      ee' <- switchHoldPromptly never ee :: HydrationDomBuilderT
                        GhcjsDomSpace
                        DomTimeline
                        (DomCoreWidget x)
                        (Event (SpiderTimeline Global) [Position])

      dynExpression' <- foldDyn
                           (\ps oldExpr -> insertMarkAt oldExpr (head ps))
                           expr
                           ee' :: HydrationDomBuilderT
                                  GhcjsDomSpace
                                  DomTimeline
                                  (DomCoreWidget x)
                                  (Dynamic (SpiderTimeline Global) Expr)

      el "div" $ display =<< (holdDyn ["Los geht's!"] ee')
      return e

  return ()
   where css = $(embedFile "css/mark.css")

-- testWidget :: (DomBuilder t m, PostBuild t m) => m ()
-- testWidget = do
--               rg <- rangeInput (def &
--                  rangeInputConfig_initialValue .~ 16)
--               let xv = floor <$> _rangeInput_value rg :: Dynamic t Int
--               display xv
--               e3 <- dyn $ div1 <$> (+1) <$> xv :: HydrationDomBuilderT
--                         GhcjsDomSpace
--                         DomTimeline
--                         (DomCoreWidget x)
--                         (Event t (Event t [Int]))
--               dynText $ (T.pack . show) <$> xv
--               e3' <- switchHoldPromptly never e3 :: HydrationDomBuilderT
--                         GhcjsDomSpace
--                         DomTimeline
--                         (DomCoreWidget x)
--                         (Event t [Int])
--               dynText =<< foldDyn (\a _ -> (T.pack . show $ a)) "let's go!" e3'

div1 :: forall m t. (DomBuilder t m, PostBuild t m) => Int -> m (Event t [Int])
div1 i = if i > 1
    then do
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

