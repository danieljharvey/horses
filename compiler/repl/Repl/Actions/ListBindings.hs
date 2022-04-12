{-# LANGUAGE OverloadedStrings #-}

module Repl.Actions.ListBindings
  ( doListBindings,
  )
where

import Control.Monad (join)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project
  ( getCurrentBindings,
    getCurrentTypeBindings,
  )
import Language.Mimsa.Store.Substitutor
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Repl.ReplM

getTypesFromStore ::
  Store ann ->
  TypeBindings ->
  Set DataType
getTypesFromStore (Store items') (TypeBindings tBindings) =
  S.fromList $
    join $ do
      (_, hash) <- M.toList tBindings
      let getDt (StoreExpression expr' _ _) =
            case expr' of
              (MyData _ dt _) -> Just dt
              _ -> Nothing
      case M.lookup hash items' >>= getDt of
        Just item -> pure [item]
        _ -> pure []

doListBindings :: Project Annotation -> Text -> ReplM (Error Annotation) ()
doListBindings project input = do
  let showBind (name, StoreExpression expr _ _) =
        case Actions.run project (Actions.typecheckExpression project input expr) of
          Right (_, _, resolvedExpr) ->
            replOutput (prettyPrint name <> " :: " <> prettyPrint (reMonoType resolvedExpr))
          _ -> pure ()
  traverse_
    showBind
    ( getExprPairs
        (prjStore project)
        (getCurrentBindings $ prjBindings project)
    )
  let showType dt = replOutput (prettyPrint dt)
  traverse_
    showType
    ( getTypesFromStore
        (prjStore project)
        (getCurrentTypeBindings $ prjTypeBindings project)
    )
