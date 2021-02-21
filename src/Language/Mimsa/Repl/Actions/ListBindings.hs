{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.ListBindings
  ( doListBindings,
  )
where

import Data.Foldable (traverse_)
import Data.Text (Text)
import Language.Mimsa.Actions
import Language.Mimsa.Monad
import Language.Mimsa.Printer
import Language.Mimsa.Project
  ( getCurrentBindings,
    getCurrentTypeBindings,
  )
import Language.Mimsa.Repl.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

doListBindings :: Project Annotation -> Text -> MimsaM (Error Annotation) ()
doListBindings env input = do
  let showBind (name, StoreExpression expr _ _) =
        case getTypecheckedStoreExpression input env expr of
          Right (ResolvedExpression type' _ _ _ _) ->
            logInfo (prettyPrint name <> " :: " <> prettyPrint type')
          _ -> pure ()
  traverse_
    showBind
    ( getExprPairs
        (prjStore env)
        (getCurrentBindings $ prjBindings env)
    )
  let showType dt = logInfo (prettyPrint dt)
  traverse_
    showType
    ( getTypesFromStore
        (prjStore env)
        (getCurrentTypeBindings $ prjTypeBindings env)
    )
