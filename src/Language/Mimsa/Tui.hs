{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Tui
  ( goTui,
    ExpressionInfo (..),
  )
where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as BT
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Language.Mimsa.Actions (evaluateStoreExpression)
import Language.Mimsa.Store
import Language.Mimsa.Types
  ( Expr (..),
    MonoType (..),
    Name (..),
    Printer,
    ResolvedDeps (..),
    Store (..),
    StoreEnv (..),
    StoreExpression (..),
    prettyPrint,
  )

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

highlight :: A.AttrName
highlight = "highlight"

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
  let selStr s =
        if sel
          then withAttr customAttr (str s)
          else str s
   in C.hCenter (selStr $ show a)

setUIState :: TuiState -> UIState -> TuiState
setUIState tui ui = tui {uiState = ui}

appEvent :: TuiState -> BT.BrickEvent () e -> BT.EventM () (BT.Next TuiState)
appEvent tuiState (BT.VtyEvent e) =
  case uiState tuiState of
    TuiError _ ->
      case e of
        V.EvKey V.KEsc [] -> M.halt tuiState
        _ -> M.continue tuiState
    (ViewBindings deps l) ->
      case e of
        V.EvKey V.KEsc [] -> M.halt tuiState
        ev ->
          M.continue <$> setUIState tuiState
            <$> ViewBindings deps =<< (L.handleListEventVi L.handleListEvent) ev l
appEvent tui _ = M.continue tui

drawListWithTitle :: (Show a) => String -> L.List () a -> Widget ()
drawListWithTitle title l = ui
  where
    label = str title
    ui =
      B.borderWithLabel label
        $ vLimit 30
        $ hLimit 50
        $ L.renderList listDrawElement True l

centeredBox :: [Widget a] -> Widget a
centeredBox items = C.vCenter $ vBox (C.hCenter <$> items)

drawInfo :: String -> Widget ()
drawInfo = str

drawPretty :: (Printer a) => a -> Widget ()
drawPretty = str . T.unpack . prettyPrint

drawUI :: TuiState -> [Widget ()]
drawUI tuiState =
  case uiState tuiState of
    TuiError err -> [drawError err]
    (ViewBindings deps l) -> do
      let store' = store . storeEnv $ tuiState
      pure (drawBindingsList store' deps l)

drawBindingsList :: Store -> ResolvedDeps -> L.List () Name -> Widget ()
drawBindingsList store' deps l = twoColumnLayout "Current scope" left right
  where
    left = C.vCenter $ drawListWithTitle "List of items" l
    right =
      C.vCenter
        ( fromMaybe
            (drawInfo "-")
            (drawExpressionInfo <$> getExpressionForBinding store' deps l)
        )

drawError :: UIError -> Widget ()
drawError (MissingStoreItems missing) =
  centeredBox
    ( [ drawInfo "Could not find expressions in the store for the following bindings: "
      ]
        <> ( drawPretty
               <$> missing
           )
    )

data ExpressionInfo
  = ExpressionInfo
      { eiType :: MonoType,
        eiExpr :: Expr Name,
        eiName :: Name
      }

drawExpressionInfo :: ExpressionInfo -> Widget ()
drawExpressionInfo exprInfo = ui
  where
    label = str . T.unpack . prettyPrint . eiName $ exprInfo
    ui =
      B.borderWithLabel label
        $ vLimit 30
        $ hLimit 50
        $ items
    items =
      centeredBox
        [ withAttr "highlight" $ drawInfo "type",
          drawPretty (eiType exprInfo),
          B.hBorder,
          withAttr "highlight" $ drawInfo "expression",
          drawPretty (eiExpr exprInfo)
        ]

evaluateStoreExprToInfo :: Store -> StoreExpression -> Maybe (MonoType, Expr Name)
evaluateStoreExprToInfo store' storeExpr =
  case evaluateStoreExpression store' storeExpr of
    Right (mt, _, _, _) -> Just (mt, storeExpression storeExpr)
    _ -> Nothing

getExpressionForBinding :: Store -> ResolvedDeps -> L.List () Name -> Maybe ExpressionInfo
getExpressionForBinding store' (ResolvedDeps deps) l =
  case L.listSelectedElement l of
    Just (_, name) -> M.lookup name deps
      >>= \storeExpr' ->
        (\(mt, expr) -> ExpressionInfo mt expr name)
          <$> evaluateStoreExprToInfo store' storeExpr'
    _ -> Nothing

twoColumnLayout :: String -> Widget a -> Widget a -> Widget a
twoColumnLayout title left right =
  Brick.withBorderStyle B.unicode
    $ B.borderWithLabel (str title)
    $ (centeredBox [left] <+> B.vBorder <+> centeredBox [right])

resolvedDepsToList :: ResolvedDeps -> Int -> L.List () Name
resolvedDepsToList (ResolvedDeps deps) i =
  L.list () (Vec.fromList $ M.keys deps) i

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, V.white `on` V.blue),
      (L.listSelectedAttr, V.blue `on` V.white),
      (customAttr, fg V.cyan),
      (highlight, fg V.cyan)
    ]

theApp :: M.App (TuiState) e ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }

data TuiState
  = TuiState
      { storeEnv :: StoreEnv,
        uiState :: UIState
      }

data UIError
  = MissingStoreItems [Name]

data UIState
  = TuiError UIError
  | ViewBindings ResolvedDeps (L.List () Name)

-- if all the deps are in place, we start by showing all the bound items
-- in the project
initialState :: StoreEnv -> TuiState
initialState storeEnv' =
  TuiState
    { uiState = initialUiState,
      storeEnv = storeEnv'
    }
  where
    initialUiState =
      case resolveDeps (store storeEnv') (bindings storeEnv') of
        Right resolvedDeps ->
          ViewBindings resolvedDeps (resolvedDepsToList resolvedDeps 0)
        Left missing -> TuiError (MissingStoreItems missing)

goTui :: StoreEnv -> IO StoreEnv
goTui storeEnv' =
  do
    _ <- M.defaultMain theApp (initialState storeEnv')
    pure storeEnv'
