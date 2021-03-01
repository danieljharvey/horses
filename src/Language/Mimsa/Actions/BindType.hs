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
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

-- add a new type to the project, creating any functions for it as we go
bindType ::
  Text ->
  DataType ->
  Actions.ActionM ([Typeclass], Maybe (ResolvedExpression Annotation), DataType)
bindType input dt = do
  addTypeToProject input dt
  let name = tyConToName (dtName dt)
  project <- Actions.getProject
  codegenExpr <- createCodegenFunction project dt
  Actions.appendMessage
    ( "Bound type " <> prettyPrint (dtName dt) <> "."
    )
  case codegenExpr of
    Nothing -> do
      pure (mempty, Nothing, dt)
    Just codegenFunc ->
      do
        Actions.bindStoreExpression (storeExprFromResolved codegenFunc) name
        Actions.appendMessage
          ( "Generated functions bound to " <> prettyPrint name <> "."
          )
        pure
          ( typeclassMatches dt,
            Just codegenFunc,
            dt
          )

storeExprFromResolved :: ResolvedExpression ann -> StoreExpression ann
storeExprFromResolved (ResolvedExpression _ se _ _ _) = se

addTypeToProject :: Text -> DataType -> Actions.ActionM ()
addTypeToProject input dt = do
  project <- Actions.getProject
  -- create storeExpr for new datatype
  resolvedTypeExpr <-
    liftEither $
      getTypecheckedStoreExpression
        input
        project
        ( MyData
            mempty
            dt
            (MyLiteral mempty (MyUnit mempty))
        )
  Actions.bindTypeExpression (reStoreExpression resolvedTypeExpr)

createCodegenFunction ::
  Project Annotation ->
  DataType ->
  Actions.ActionM (Maybe (ResolvedExpression Annotation))
createCodegenFunction project dt =
  case doCodegen dt of
    items | M.null items -> pure Nothing
    funcMap -> do
      storeExprs <-
        traverse
          ( \(name, expr) -> do
              resolvedExpr <-
                liftEither $
                  getTypecheckedStoreExpression
                    (prettyPrint expr)
                    project
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
      let recordExpr = MyRecord mempty realFunctionMap
      re <-
        liftEither $ getTypecheckedStoreExpression (prettyPrint recordExpr) (project <> newProjectItems) recordExpr
      pure (Just re)
