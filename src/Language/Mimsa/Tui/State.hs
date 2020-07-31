{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Tui.State
  ( appEvent,
    initialState,
  )
where

import qualified Brick.Main as M
import qualified Brick.Types as BT
import qualified Brick.Widgets.List as L
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Language.Mimsa.Store (getCurrentBindings)
import Language.Mimsa.Store.ResolvedDeps
import Language.Mimsa.Tui.Evaluate
import Language.Mimsa.Types

-- we don't want this to do much so binning it in exchange for an Elm style
-- reducer pattern
appEvent :: TuiState -> BT.BrickEvent () e -> BT.EventM () (BT.Next TuiState)
appEvent tuiState (BT.VtyEvent e) = do
  let tuiAction = case e of
        V.EvKey V.KEsc [] -> Exit
        V.EvKey V.KLeft [] -> GoLeft
        V.EvKey V.KRight [] -> GoRight
        V.EvKey V.KEnter [] -> GoRight
        V.EvKey V.KUp [] -> GoUp
        V.EvKey V.KDown [] -> GoDown
        _ -> Unknown
  case reducer (project tuiState) tuiAction (uiState tuiState) of
    Just newState -> M.continue $ tuiState {uiState = newState}
    _ -> M.halt tuiState
appEvent tui _ = M.continue tui

-----

-- instead of dealing with the full Vty events, here is a smaller subset we care
-- about
data TuiAction
  = Exit -- generally, the Escape key
  | GoLeft
  | GoRight
  | GoUp
  | GoDown
  | Unknown

reducer :: Project -> TuiAction -> UIState -> Maybe UIState
reducer _ Exit _ = Nothing
reducer _ GoUp (ViewBindings items) =
  let mapF (BindingsList n deps l) = BindingsList n deps (L.listMoveUp l)
   in Just (ViewBindings (mapHead mapF items))
reducer _ GoDown (ViewBindings items) =
  let mapF (BindingsList n deps l) = BindingsList n deps (L.listMoveDown l)
   in Just (ViewBindings (mapHead mapF items))
reducer project' GoRight (ViewBindings items) =
  let topItem = NE.head items
   in case getExpressionForBinding (store project') (bDeps topItem) (bList topItem) of
        Just (ExpressionInfo _ _ newName newDeps)
          | not (hasNoDeps newDeps) ->
            let newItem = makeBindingsList newName newDeps
             in Just (ViewBindings (NE.cons newItem items))
        _ -> Just (ViewBindings items)
reducer _ GoLeft (ViewBindings items) =
  let newItems =
        if NE.length items > 1
          then NE.fromList (NE.tail items)
          else items
   in Just (ViewBindings newItems)
reducer _ _ _ = Nothing

mapHead :: (a -> a) -> NonEmpty a -> NonEmpty a
mapHead f list = NE.fromList ([hd] <> tl)
  where
    hd = f (NE.head list)
    tl = NE.tail list

-- if all the deps are in place, we start by showing all the bound items
-- in the project
initialState :: Project -> TuiState
initialState project' =
  TuiState
    { uiState = initialUiState,
      project = project'
    }
  where
    initialUiState =
      case resolveDeps (store project') (getCurrentBindings $ bindings project') of
        Right resolvedDeps ->
          ViewBindings $
            pure
              ( makeBindingsList
                  (Name "Root project")
                  resolvedDeps
              )
        Left missing -> TuiError (MissingStoreItems missing)

makeBindingsList :: Name -> ResolvedDeps -> BindingsList
makeBindingsList name deps =
  BindingsList name deps (resolvedDepsToList deps 0)
  where
    resolvedDepsToList (ResolvedDeps deps') i =
      L.list () (Vec.fromList $ M.keys deps') i
