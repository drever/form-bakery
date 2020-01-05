{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Markup (parseAndRenderWidget
             , consequence
             , expression
             , parseError
             , truthTable
             , expressionWidget

             , highlight
             , id'
             , toc
             , section
             , p

             , c0
             , theRangeTester
             , dragTester

             , SubSection (..)) where

import Control.Applicative
import Common
import Reflex.Dom
import Reflex (Dynamic)
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T
import Control.Lens ((%~), (^?), (+~), (?~), (#), from, at)
import Reflex.Dom.Core (MonadWidget, (=:))
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))

id' :: T.Text -> Map.Map T.Text T.Text
id' x = mempty & at "id" ?~ x

toc :: (DomBuilder t m) => [SubSection m] -> m ()
toc xs = el "ul" $ mapM_ (\(SubSection t a _) ->
                el "li" $ elAttr' "a" (mempty & at "href" ?~ ("#" <> a)) $ text t) xs

p :: (DomBuilder t m) => T.Text -> T.Text -> m() -> m ()
p t a c = do
  elAttr "h3" (id' a) $ text t
  el "p" c

data SubSection m = SubSection { title :: T.Text, subSectionId :: T.Text, content :: m () }

section :: (DomBuilder t m) => [SubSection m] -> m ()
section xs = do toc xs
                mapM_ (\(SubSection t a c) -> p t a c) xs

expressionWidget :: forall t m. (DomBuilder t m, MonadFix m, PostBuild t m, MonadHold t m)
                 => Expr
                 -> m (Dynamic t Expr)
expressionWidget expr = do
  rec
      dynExpression' <- foldDyn insertMarkAt expr
                        =<< switchHold never
                        =<< dyn (expression <$> dynExpression')
  return dynExpression'

parseAndRenderWidget :: forall t m. (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, MonadHold t m)
                     => T.Text
                     -> m ()
parseAndRenderWidget e = elClass "div" "parseAndRender" $ do
      rec
          text <- do switchedText <- switchHold never (updated <$> newText)
                     inputElement $ def
                               & inputElementConfig_initialValue .~ e
                               & inputElementConfig_setValue .~ switchedText

          newText <- (dyn $ let failure err = do parseError err
                                                 return (_inputElement_value text)
                                success = ((T.pack . show <$>)<$>) . truthTable
                                parsedExpression = parseExpr <$> _inputElement_value text
                             in either failure
                                       success
                                       <$> parsedExpression) :: m (Event t (Dynamic t T.Text))
      return ()

consequence :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
            => T.Text
            -> T.Text
            -> m ()
consequence l r = do
  elClass "div" "consequence" $ do
      parseAndRenderWidget l
      parseAndRenderWidget r
      return ()


highlight txt = elAttr "span" (mempty & at "id" ?~ "code-highlight") $ text txt

parseError :: (DomBuilder t m) => T.Text -> m ()
parseError err = elClass "div" "errormessage" $ text ("The entered expression is not correct. Only  ,<> and <...> and character variables are allowed. Reason: " <> err)

truthTable :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
      => Expr
      -> m (Dynamic t Expr)
truthTable e = elClass "div" "truthtable" $
                case listValues e of
                 [] -> (text "") >> (return $ constDyn $ Call [])
                 vs -> do el "table" $ do
                             expr <- el "tr" $
                                       do let ns = map fst . Map.toList . fst . head $ vs :: [Char]
                                          mapM_ (\v -> el "th" (text (T.pack . (:[]) $ v))) ns
                                          elClass "th" "result" (expressionWidget e)
                             mapM_ (\vs'' -> el "tr" $ do let vs' = map snd . Map.toList . fst $ vs'' :: [Expr]
                                                          mapM_ (\v -> el "td" (expression $ v)) (vs' :: [Expr])
                                                          elClass "td" "result" (expression . snd $ vs'')) (vs :: [(Env, Expr)])
                             return expr

data ExpressionClass =
          UnmarkedExpr
        | VarExpr
        | CrossExpr
        | CallExpr

instance Show ExpressionClass where
    show UnmarkedExpr = "unmarked"
    show VarExpr = "var"
    show CrossExpr = "cross"
    show CallExpr = "call"

expression :: DomBuilder t m
      => Expr
      -> m (Event t Position)
expression = (fmap (fmap head)) . snd . (expression' "0")
    where expression' :: DomBuilder t m
                         => Position
                         -> Expr
                         -> (T.Text, m (Event t [Position]))
          expression' p (Call []) = let p' = p <> "B"
                                     in (p', divButton UnmarkedExpr p' (blank >> return never))
          expression' p (Var e) = let p' = p <> "B"
                                   in (p', divButton VarExpr p' (text (T.pack $ e:[]) >> return never))
          expression' p (Call es) =
                let subExprs = mapM (\(i, e) -> snd $ expression' (p <> (T.pack . show $ i)) e)
                                     (zip [0..] es)
                 in (p, divButton CallExpr p (leftmost <$> subExprs))
          expression' p (Cross e) = let p' = (p <> "C")
                                     in (p', do let subExpr = snd $ expression' p' e
                                                    b = divButton CrossExpr p subExpr
                                                b)

          divButton :: forall t m. DomBuilder t m
                    => ExpressionClass
                    -> Position
                    -> m (Event t [Position])
                    -> m (Event t [Position])
          divButton c cs e = do
                    let attrs = mempty & at "class" ?~ (T.pack . show $ c)
                                  & at "data-depth" ?~ cs
                        cfg = def :: ElementConfig EventResult t (DomBuilderSpace m)

                    -- (t, ev) <- elAttr' "div" attrs e
                    (t, ev) <- element "div" (cfg & initialAttributes .~ attrs
                                                  & elementConfig_eventSpec %~ addEventSpecFlags
                                                               (Proxy :: Proxy (DomBuilderSpace m))
                                                               Click
                                                               (const stopPropagation)
                                                               ) e
                    let de = const [cs] <$> domEvent Click t
                    return $ ev <> de

-- testing
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

theRangeTester :: forall t m. (DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace) =>
                 m ()
theRangeTester = do
  rg <- rangeInput (def & rangeInputConfig_initialValue .~ 16)
  let xv = floor <$> _rangeInput_value rg
  display xv
  e3 <- dyn $ div1 <$> (+1) <$> xv
  return ()

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

data DragStatus = BeginRect Int Int
                | SizeRect Int Int
                | EndRect Int Int

data RectState = RectState {
    x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  , isDragging :: Bool
}

fromRectState (RectState x y w h i) = ((x, y), (w, h))

dragTest :: forall t m. (DomBuilder t m, MonadFix m, PostBuild t m, MonadHold t m) =>
            m (Dynamic t ((Int, Int), (Int, Int)))
dragTest = do
     rec
              (t1, _) <- elAttr' "div" (mempty & at "class" ?~ "test-class") blank
              elDynAttr "div" (rectSize <$> s) blank

              s <- foldDyn (flip updateRectState)
                           (RectState 0 0 0 0 False)
                           (leftmost [ uncurry BeginRect <$> domEvent Mousedown t1
                                     , uncurry EndRect <$> domEvent Mouseup t1
                                     , uncurry SizeRect <$> domEvent Mousemove t1]) :: m (Dynamic t RectState)

     return (fromRectState <$> s)

      where updateRectState s = \case
               BeginRect x y -> RectState x y 0 0 True
               SizeRect x' y' -> case isDragging s of
                                         True -> s { width = x' - x s
                                                   , height = y' - y s }
                                         False -> s
               EndRect x' y' -> s { isDragging = False }
            rectSize :: RectState -> Map.Map T.Text T.Text
            rectSize s = mempty & at "id" ?~ "drag-rect"
                            & at "style" ?~ T.intercalate ";" [
                                  "top: " <> (T.pack . show . x $ s) <> "px"
                                , "left: " <> (T.pack . show . y $ s) <> "px"
                                , "width: " <> (T.pack . show . width $ s) <> "px"
                                , "height: " <> (T.pack . show .height $ s) <> "px"
                                , "position: absolute"
                                , "display: " <> case isDragging s of
                                                  True -> "block"
                                                  False -> "none"]


dragTester :: forall t m. (DomBuilder t m, MonadFix m, PostBuild t m, MonadHold t m) =>
              m ()
dragTester = do x <- dragTest
                display x
                return ()

