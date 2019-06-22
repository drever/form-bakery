{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Markup (expression
             , parseError
             , truthTable) where

import Common
import Reflex.Dom
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T

parseError err = (text . T.pack . show $ err) >> (return never)

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
      -> m (Event t ())
expression e = snd . (expression' (0, 0))
     -- let ((w, h), b) = expressionSvg e
     --     viewBox = T.unwords ["0", "0", T.pack . show $ w, T.pack . show $ h]
      -- in elAttr "svg" ("viewBox" =: viewBox) (b (0, 0))

expression' :: DomBuilder t m
          => (Int, Int)
          -> Expr
          -> ((Int, Int), m (Event t ()))
expression' (i, j) (Call []) = (
            (i, j)
          , divButton
                "unmarked" (T.pack $ show (i, j))
                (text ""))

expression' (i, j) (Var e) = (
              (i, j)
           , divButton
                "var" (T.pack $ show (i, j))
                (text (T.pack $ e:[])))

expression' (i, j) (Call es) = (
            (i, j)
          , divButton
                "call" (T.pack $ show (i, j))
                (mapM_ ((\(j', e) -> snd $ expression' (i, j') e)) (zip [1..] es)))

expression' (i, j) (Cross e) = (
            (i, j)
          , divButton
                "cross" (T.pack $ show (i, j))
                (snd $ expression' (i + 1, j) e))

divButton :: DomBuilder t m
          => T.Text
          -> T.Text
          -> m a
          -> m (Event t ())
divButton c cs e = do
          (e', _) <- elClass' "div" c e
          return $ domEvent Click e'

type Position = (Int, Int)
type Size = (Int, Int)

baseSize = 20

lineAttr :: Int -> Int -> Int -> Int -> T.Text -> Map.Map T.Text T.Text
lineAttr x1 y1 x2 y2 stroke = Map.fromList [("x1", T.pack . show $ x1), ("y1", T.pack . show $ y1), ("x2", T.pack . show $ x2), ("y2", T.pack . show $ y2), ("stroke", stroke)]

expressionSvg :: DomBuilder t m
          => Expr
          -> (Size, Position -> m (Event t ()))
expressionSvg (Var v) =
  ((baseSize, baseSize),
   \(x, y) ->
       let viewBox = T.unwords [T.pack . show $ x
                              , T.pack . show $ y
                              , T.pack . show $ baseSize
                              , T.pack . show $ baseSize]
       in do (e, _) <- elAttr' "g" ("viewBox" =: viewBox ) $ el "text" (text $ T.pack $ v:[])
             return $ domEvent Click e)

expressionSvg (Call []) =
  ((baseSize, baseSize),
   \(x, y) ->
       let viewBox = T.unwords [T.pack . show $ x
                              , T.pack . show $ y
                              , T.pack . show $ baseSize
                              , T.pack . show $ baseSize]
   in do (e, _) <- elAttr' "g" ("viewBox" =: viewBox ) $ blank
         return $ domEvent Click e)

expressionSvg (Cross e) =
  let ((sw, sh), subExpressionBuilder) = expressionSvg e
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

expressionSvg (Call es) =
   let builders = map expressionSvg es
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


