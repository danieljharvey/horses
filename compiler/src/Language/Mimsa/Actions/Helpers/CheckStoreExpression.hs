module Language.Mimsa.Actions.Helpers.CheckStoreExpression where

import Data.Text (Text)
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

-- | re-check a store expression. this could be because we want it annotated
-- with types, or because we've changed the deps and want to see if it still
-- works
checkStoreExpression ::
  Text ->
  Project Annotation ->
  StoreExpression Annotation ->
  Actions.ActionM (ResolvedExpression Annotation)
checkStoreExpression input _project se = do
  --  let project' = project <> fromStoreExpressionDeps se

  Actions.typecheckStoreExpression se input

-- have broken this because it makes no sense with StoreDataType --?
