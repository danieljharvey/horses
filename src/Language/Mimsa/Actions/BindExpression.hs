{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.BindExpression (bindExpression) where

import Control.Monad.Except (liftEither)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Language.Mimsa.Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Project.UnitTest
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

-- bind a new expression to the project, updating any tests

bindExpression ::
  Expr Name Annotation ->
  Name ->
  Text ->
  Actions.ActionM Int
bindExpression expr name input = do
  project <- Actions.getProject
  (ResolvedExpression _type' storeExpr _ _ _) <-
    liftEither $ getTypecheckedStoreExpression input project expr
  Actions.bindStoreExpression storeExpr name
  case lookupBindingName project name of
    Nothing -> do
      Actions.appendMessage
        ( "Bound " <> prettyPrint name <> "."
        )
      pure 0
    Just oldExprHash -> do
      Actions.appendMessage
        ( "Updated binding of " <> prettyPrint name <> "."
        )
      let newExprHash = getStoreExpressionHash storeExpr
      createUnitTests oldExprHash newExprHash

createUnitTests :: ExprHash -> ExprHash -> Actions.ActionM Int
createUnitTests oldExprHash newExprHash = do
  project <- Actions.getProject
  (projectWithNewTests, newExprs) <-
    liftEither $
      createNewUnitTests project oldExprHash newExprHash
  traverse_ Actions.addStoreExpression newExprs
  Actions.setProject projectWithNewTests
  pure (length newExprs)
