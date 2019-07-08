{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}

module Markup (expression
             , parseError
             , expressionSVG
             , truthTable) where

import Control.Applicative
import Common
import Reflex.Dom
import Reflex (Dynamic)
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Reflex.Dom.Widget.SVG as S
import Reflex.Dom.Widget.SVG.Types (SVG_Rect)
import qualified Reflex.Dom.Widget.SVG.Types as S
import Control.Lens ((^?), (+~), (?~), (#), from, at)
import Reflex.Dom.Core (MonadWidget, (=:))
import qualified Data.List.NonEmpty as NE

parseError :: (DomBuilder t m, Show a) => a -> m [Event t b]
parseError err = (text . T.pack . show $ err) >> (return [never])


truthTable :: DomBuilder t m
      => Expr
      -> m (Event t ())
truthTable e = elClass "div" "truthtable" $ do
                case listValues e of
                 [] -> (text "") >>  return never
                 vs -> do el "table" $ do
                             el "tr" $ do let ns = map fst . Map.toList . fst . head $ vs :: [Char]
                                          mapM_ (\v -> el "th" (text (T.pack . (:[]) $ v))) ns
                                          el "th" (expression e)
                             mapM_ (\vs'' -> el "tr" $ do let vs' = map snd . Map.toList . fst $ vs'' :: [Expr]
                                                          mapM_ (\v -> el "td" (expression $ v)) (vs' :: [Expr])
                                                          el "td" (expression . snd $ vs'')) (vs :: [(Env, Expr)])
                          return never

expression :: DomBuilder t m
      => Expr
      -> m [Event t Position]
expression = snd . (expression' (0, 0))
    where expression' :: DomBuilder t m
                         => (Int, Int)
                         -> Expr
                         -> ((Int, Int), m [Event t Position])
          expression' p (Call []) = (p, divButton "unmarked" p (blank >> return [never]))
          expression' p (Var e) = (p, divButton "var" p (text (T.pack $ e:[]) >> return [never]))
          expression' p@(i, _) (Call es) =
                let subExprs = mapM (\(j', e) -> snd $ expression' (i, j') e)
                                     (zip [1..] es)
                 in (p, divButton "call" p (join <$> subExprs))
          expression' p@(i, j) (Cross e) = (p, do let subExpr = snd $ expression' (i + 1, j) e
                                                      b = divButton "cross" p subExpr
                                                  b)

          divButton :: DomBuilder t m
                    => T.Text
                    -> Position
                    -> m [Event t Position]
                    -> m [Event t Position]
          divButton c cs e = do
                    let attrs = mempty & at "class" ?~ c
                                  & at "data-depth" ?~ (T.pack . show $ cs)
                                  & at "z-index" ?~ (T.pack . show . (*(-1)) . fst $ cs)
                    (t, ev) <- elAttr' "div" attrs e
                    let de = const cs <$> domEvent Click t
                    return $ de:ev

-- SVG
--
type Size = (Int, Int)

baseSize = 20

expressionSVG :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
                 => Expr
                 -> m (Event t ())
expressionSVG e = --snd . (expression' (0, 0))
     -- let ((w', h'), b) = expressionSvg e
     let w' = 100
         h' = 100
         w = S.Width $ fromIntegral w'
         h = S.Height $ fromIntegral h'
         viewBox = pure $ S.ViewBox 0 0 w h
         svgProps = pure $ S.SVG_El w h (viewBox)

         dRect1 = S.SVG_Rect
                    (S._PosX # 40.0)
                    (S._PosY # 40.0)
                    (S.Width 50.0)
                    (S.Height 50.0)
                    Nothing
                    Nothing

         attrs = mempty
             & at "id" ?~ "svg1"
             & at "class" ?~ "blue no-yellow"
      in do
            _ <- S.svg_ svgProps $ do
                      S.svgBasicDyn S.Rectangle (mappend attrs . S.makeRectProps) (pure dRect1) (pure mempty)
                      -- S.svgBasicDyn (expressionSvg'' e) (mappend attrs . S.makeRectProps) (pure dRect1) (pure mempty)
                     --b (0, 0)
            pure () >> (return never)

lineAttr :: Int -> Int -> Int -> Int -> T.Text -> Map.Map T.Text T.Text
lineAttr x1 y1 x2 y2 stroke = Map.fromList [("x1", T.pack . show $ x1), ("y1", T.pack . show $ y1), ("x2", T.pack . show $ x2), ("y2", T.pack . show $ y2), ("stroke", stroke)]

expressionSvg'' :: Expr -> S.BasicSVG
expressionSvg'' = undefined

expressionSvg' :: DomBuilder t m
          => Expr
          -> (Size, Position -> m (Event t ()))
expressionSvg' (Var v) =
  ((baseSize, baseSize),
   \(x, y) ->
       let viewBox = T.unwords [T.pack . show $ x
                              , T.pack . show $ y
                              , T.pack . show $ baseSize
                              , T.pack . show $ baseSize]
       in do (e, _) <- elAttr' "g" ("viewBox" =: viewBox ) $ el "text" (text $ T.pack $ v:[])
             return $ domEvent Click e)

expressionSvg' (Call []) =
  ((baseSize, baseSize),
   \(x, y) ->
       let viewBox = T.unwords [T.pack . show $ x
                              , T.pack . show $ y
                              , T.pack . show $ baseSize
                              , T.pack . show $ baseSize]
   in do (e, _) <- elAttr' "g" ("viewBox" =: viewBox ) $ blank
         return $ domEvent Click e)

expressionSvg' (Cross e) =
  let ((sw, sh), subExpressionBuilder) = expressionSvg' e
      h = sh + 5
      w = sw + 5
   in ((w, h),
       \(x, y) -> let viewBox = T.unwords [T.pack . show $ x
                                         , T.pack . show $ y
                                         , T.pack . show $ w
                                         , T.pack . show $ h]
                   in do (e', _) <- elAttr' "g" ("viewBox" =: viewBox) $ do
                                                         elAttr "line" (lineAttr x y (x + sw) y "black") blank
                                                         elAttr "line" (lineAttr (x + sw) y (x + sw) (y + sh) "black") blank
                                                         (subExpressionBuilder (x, y))
                         return $ domEvent Click e')

expressionSvg' (Call es) =
   let builders = map expressionSvg' es
       w = foldr (\((w, _), _) acc ->  acc + w) 5 builders
       h = foldr (\((_, h), _) acc ->  max acc h) 5 builders
    in ((w, h),
       \p@(x, y) -> do let viewBox = T.unwords [T.pack . show $ x
                                               , T.pack . show $ y
                                               , T.pack . show $ w
                                               , T.pack . show $ h]
                       (e', _) <- elAttr' "g" ("viewBox" =: viewBox) $ do
                                          blarec p builders
                       return $ domEvent Click e')


blarec :: DomBuilder t m => Position -> [(Size, Position -> m (Event t ()))] -> m (Event t ())
blarec (x, y) [] = blank >> (return never)
blarec (x, y) (((h, w), e):es) = do e (x, y)
                                    blarec (x + w, y) es


