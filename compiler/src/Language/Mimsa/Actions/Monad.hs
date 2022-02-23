{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Actions.Monad
  ( run,
    getProject,
    appendProject,
    appendMessage,
    appendDocMessage,
    appendWriteFile,
    appendOptimisedStoreExpression,
    setProject,
    appendStoreExpression,
    bindStoreExpression,
    bindTypeExpression,
    messagesFromOutcomes,
    storeExpressionsFromOutcomes,
    writeFilesFromOutcomes,
    ActionM,
    SavePath (..),
    SaveContents (..),
    SaveFilename (..),
  )
where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Language.Mimsa.Actions.Types
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Prettyprinter

run ::
  Project Annotation ->
  ActionM a ->
  Either (Error Annotation) (Project Annotation, [ActionOutcome], a)
run project action =
  let ((result, outcomes), newState) =
        runState (runWriterT (runExceptT action)) project
   in (,,) newState outcomes <$> result

getProject :: ActionM (Project Annotation)
getProject = get

setProject :: Project Annotation -> ActionM ()
setProject =
  put

appendProject :: Project Annotation -> ActionM ()
appendProject prj =
  modify (<> prj)

appendMessage :: Text -> ActionM ()
appendMessage =
  tell . pure . NewMessage

appendDocMessage :: Doc ann -> ActionM ()
appendDocMessage = appendMessage . renderWithWidth 50

-- | save a store expression and store which exprhash it's an optimisation of
-- so we can re-use this later
appendOptimisedStoreExpression :: ExprHash -> StoreExpression Annotation -> ActionM ()
appendOptimisedStoreExpression originalHash storeExpr = do
  let newProject = fromOptimisation originalHash storeExpr
  tell (pure (NewStoreExpression storeExpr))
  appendProject newProject

appendWriteFile ::
  SavePath ->
  SaveFilename ->
  SaveContents ->
  ActionM ()
appendWriteFile savePath filename content =
  tell $ pure $ NewWriteFile savePath filename content

appendStoreExpression :: StoreExpression Annotation -> ActionM ()
appendStoreExpression se = do
  let newProject = fromStoreExpression se (getStoreExpressionHash se)
  tell (pure (NewStoreExpression se))
  appendProject newProject

messagesFromOutcomes :: [ActionOutcome] -> [Text]
messagesFromOutcomes =
  foldMap
    ( \case
        NewMessage tx -> pure tx
        _ -> mempty
    )

storeExpressionsFromOutcomes :: [ActionOutcome] -> Set (StoreExpression Annotation)
storeExpressionsFromOutcomes =
  S.fromList
    . foldMap
      ( \case
          NewStoreExpression se -> pure se
          _ -> mempty
      )

writeFilesFromOutcomes :: [ActionOutcome] -> [(SavePath, SaveFilename, SaveContents)]
writeFilesFromOutcomes =
  foldMap
    ( \case
        NewWriteFile sp sf sc -> pure (sp, sf, sc)
        _ -> mempty
    )

-- add binding for expression and add it to store
bindStoreExpression ::
  StoreExpression Annotation ->
  Name ->
  ActionM ()
bindStoreExpression storeExpr name = do
  appendStoreExpression storeExpr
  appendProject
    ( fromItem name storeExpr (getStoreExpressionHash storeExpr)
    )

bindTypeExpression ::
  StoreExpression Annotation -> ActionM ()
bindTypeExpression storeExpr = do
  -- add the expression to the store
  appendStoreExpression storeExpr
  -- add the type to the project
  appendProject $
    fromType
      storeExpr
      (getStoreExpressionHash storeExpr)
