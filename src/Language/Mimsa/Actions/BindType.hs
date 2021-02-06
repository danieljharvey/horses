{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.BindType
  ( bindType,
  )
where

import Control.Monad.Except (liftEither)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store
import Language.Mimsa.Typechecker.Codegen
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

-- add a new type to the project, creating any functions for it as we go
bindType ::
  Text ->
  DataType ->
  Name ->
  Actions.ActionM [Typeclass]
bindType input dt name = do
  project <- Actions.getProject
  storeExpr <- createStoreExpressions input dt
  let typeclasses = typeclassMatches dt
  Actions.bindStoreExpression storeExpr name
  case lookupBindingName project name of
    Nothing -> do
      Actions.appendMessage
        ( "Bound type " <> prettyPrint name <> "."
        )
      pure typeclasses
    Just _oldExprHash ->
      do
        Actions.appendMessage
          ( "Updated type binding of " <> prettyPrint name <> "."
          )
        pure typeclasses

createStoreExpressions :: Text -> DataType -> Actions.ActionM (StoreExpression Annotation)
createStoreExpressions input dt = do
  project <- Actions.getProject
  let funcMap = doCodegen dt
  resolvedTypeExpr <-
    liftEither $
      getTypecheckedStoreExpression input project (MyData mempty dt (MyLiteral mempty (MyUnit mempty)))
  let projectWithType =
        project
          <> fromType
            (reStoreExpression resolvedTypeExpr)
            (getStoreExpressionHash (reStoreExpression resolvedTypeExpr))
  storeExprs <-
    traverse
      ( \(name, expr) -> do
          resolvedExpr <-
            liftEither $
              getTypecheckedStoreExpression
                (prettyPrint expr)
                projectWithType
                expr
          Actions.appendStoreExpression (reStoreExpression resolvedExpr)
          pure
            (name, reStoreExpression resolvedExpr)
      )
      (M.toList funcMap)
  -- create project items for use in our record type
  let newProjectItems =
        mconcat
          ( ( \(name, expr) ->
                fromItem
                  name
                  expr
                  (getStoreExpressionHash expr)
            )
              <$> storeExprs
          )
  -- add these new expressions (but not their bindings) to the project Store
  Actions.appendProject
    ( mconcat
        ( ( \se ->
              fromStoreExpression
                se
                (getStoreExpressionHash se)
          )
            . snd
            <$> storeExprs
        )
    )
  let realFunctionMap = M.mapWithKey (\k _ -> MyVar mempty k) funcMap
  let recordExpr = MyData mempty dt (MyRecord mempty realFunctionMap)
  (ResolvedExpression _ storeExpr _ _ _) <-
    liftEither $ getTypecheckedStoreExpression input (projectWithType <> newProjectItems) recordExpr
  pure storeExpr
