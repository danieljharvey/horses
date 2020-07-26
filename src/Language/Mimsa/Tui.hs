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
import Language.Mimsa.Types
  ( Bindings (..),
    Expr (..),
    MonoType (..),
    Name (..),
    Printer,
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
    NoProject ->
      case e of
        V.EvKey V.KEsc [] -> M.halt (setUIState tuiState NoProject)
        _ -> M.continue (setUIState tuiState NoProject)
    (ListScope l) ->
      case e of
        V.EvKey V.KEsc [] -> M.halt $ setUIState tuiState (ListScope l)
        ev ->
          M.continue <$> setUIState tuiState
            <$> ListScope =<< (L.handleListEventVi L.handleListEvent) ev l
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
    NoProject -> [C.vCenter $ vBox [str "No project"]]
    (ListScope l) ->
      pure $ twoColumnLayout "Current scope" left right
      where
        left = C.vCenter $ drawListWithTitle "List of items" l
        right =
          C.vCenter
            ( fromMaybe
                (drawInfo "-")
                (drawExpressionInfo <$> getExpressionForBinding tuiState)
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

getStoreExprByName :: Name -> StoreEnv -> Maybe StoreExpression
getStoreExprByName name (StoreEnv (Store items) (Bindings bindings')) =
  M.lookup name bindings' >>= (\exprHash -> M.lookup exprHash items)

evaluateStoreExprToInfo :: StoreEnv -> StoreExpression -> Maybe (MonoType, Expr Name)
evaluateStoreExprToInfo (StoreEnv store' _) storeExpr =
  case evaluateStoreExpression store' storeExpr of
    Right (mt, _, _, _) -> Just (mt, storeExpression storeExpr)
    _ -> Nothing

getExpressionForBinding :: TuiState -> Maybe ExpressionInfo
getExpressionForBinding (TuiState storeEnv' ui) = case ui of
  (ListScope l) -> case L.listSelectedElement l of
    Just (_, name) -> getStoreExprByName name storeEnv'
      >>= \storeExpr' ->
        (\(mt, expr) -> ExpressionInfo mt expr name)
          <$> evaluateStoreExprToInfo storeEnv' storeExpr'
    _ -> Nothing
  _ -> Nothing

twoColumnLayout :: String -> Widget a -> Widget a -> Widget a
twoColumnLayout title left right =
  Brick.withBorderStyle B.unicode
    $ B.borderWithLabel (str title)
    $ (centeredBox [left] <+> B.vBorder <+> centeredBox [right])

storeEnvToList :: StoreEnv -> Int -> L.List () Name
storeEnvToList (StoreEnv _items (Bindings bindings')) i =
  L.list () (Vec.fromList $ M.keys bindings') i

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

data UIState
  = NoProject
  | ListScope (L.List () Name)

initialState :: StoreEnv -> TuiState
initialState storeEnv' =
  TuiState
    { uiState = ListScope $ (storeEnvToList storeEnv' 0),
      storeEnv = storeEnv'
    }

goTui :: StoreEnv -> IO StoreEnv
goTui storeEnv' =
  do
    _ <- M.defaultMain theApp (initialState storeEnv')
    pure storeEnv'
