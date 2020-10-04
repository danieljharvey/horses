{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Tui
  ( goTui,
  )
where

import Brick
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Tui.Elements
import Language.Mimsa.Tui.Evaluate
import Language.Mimsa.Tui.State
import Language.Mimsa.Tui.Styles
import Language.Mimsa.Types

drawUI :: (Eq ann, Monoid ann) => TuiState ann -> [Widget ()]
drawUI tuiState =
  case uiState tuiState of
    TuiError err -> [drawError err]
    (ViewBindings items) -> do
      let store' = store . project $ tuiState
          (BindingsList name deps l) = NE.head items
      pure (drawBindingsList store' name deps l)

drawBindingsList ::
  (Eq ann, Monoid ann) =>
  Store ann ->
  Name ->
  ResolvedDeps ann ->
  L.List () Name ->
  Widget ()
drawBindingsList store' name deps l = twoColumnLayout "Current scope" left right
  where
    left = C.vCenter $ drawListWithTitle (T.unpack $ prettyPrint name) l
    right =
      C.vCenter
        ( maybe
            (drawInfo "-")
            drawExpressionInfo
            (getExpressionForBinding store' deps l)
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

drawExpressionInfo :: ExpressionInfo ann -> Widget ()
drawExpressionInfo exprInfo = ui
  where
    label = withAttr "title" . str . T.unpack . prettyPrint . eiName $ exprInfo
    ui =
      B.borderWithLabel label
        $ vLimit 30
        $ hLimit
          50
          items
    items =
      centeredBox
        [ withAttr "highlight" $ drawInfo "type",
          drawPretty (eiType exprInfo),
          B.hBorder,
          withAttr "highlight" $ drawInfo "expression",
          drawPretty (eiExpr exprInfo),
          B.hBorder,
          withAttr "highlight" $ drawInfo "dependencies",
          drawPretty (eiDeps exprInfo)
        ]

theApp :: (Eq ann, Monoid ann) => M.App (TuiState ann) e ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return,
      M.appAttrMap = const stylesAttrMap
    }

goTui :: (Eq ann, Monoid ann) => Project ann -> IO (Project ann)
goTui project' =
  do
    _ <- M.defaultMain theApp (initialState project')
    pure project'
