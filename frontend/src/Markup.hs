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
                        =<< switchHoldPromptly never
                        =<< dyn (expression <$> dynExpression')
  return dynExpression'

parseAndRenderWidget :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
                     => T.Text
                     -> m ()
parseAndRenderWidget e = elClass "div" "parseAndRender" $ do
      t <-  inputElement $ def
           & inputElementConfig_initialValue .~ e
      elClass "div" "output" $ do
          dyn $ either (\e -> parseError e >> return never) truthTable
              . parseExpr
              <$> _inputElement_value t
      return ()

consequence :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
            => T.Text
            -> T.Text
            -> m ()
consequence l r = do
  elClass "div" "consequence" $ do
      parseAndRenderWidget l
      parseAndRenderWidget r


highlight txt = elAttr "span" (mempty & at "id" ?~ "code-highlight") $ text txt

parseError :: (DomBuilder t m) => T.Text -> m ()
parseError err = elClass "div" "errormessage" $ text ("The entered expression is not correct. Only  ,<> and <...> and character variables are allowed. Reason: " <> err)

truthTable :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
      => Expr
      -> m (Event t ())
truthTable e = elClass "div" "truthtable" $ do
                case listValues e of
                 [] -> (text "") >>  return never
                 vs -> do el "table" $ do
                             el "tr" $ do let ns = map fst . Map.toList . fst . head $ vs :: [Char]
                                          mapM_ (\v -> el "th" (text (T.pack . (:[]) $ v))) ns
                                          elClass "th" "result" ((expressionWidget e) >> return ())
                             mapM_ (\vs'' -> el "tr" $ do let vs' = map snd . Map.toList . fst $ vs'' :: [Expr]
                                                          mapM_ (\v -> el "td" (expression $ v)) (vs' :: [Expr])
                                                          elClass "td" "result" (expression . snd $ vs'')) (vs :: [(Env, Expr)])
                          return never


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

