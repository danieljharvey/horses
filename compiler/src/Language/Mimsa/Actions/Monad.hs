{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Actions.Monad
  ( run,
    getProject,
    appendProject,
    appendMessage,
    appendDocMessage,
    appendWriteFile,
    appendResolvedExpression,
    setProject,
    appendStoreExpression,
    bindStoreExpression,
    bindTypeExpression,
    messagesFromOutcomes,
    storeExpressionsFromOutcomes,
    writeFilesFromOutcomes,
    getResolvedExpressions,
    ActionM,
    SavePath (..),
    SaveContents (..),
    SaveFilename (..),
  )
where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as M
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
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Prettyprinter

emptyState :: Project Annotation -> ActionState
emptyState prj =
  ActionState
    { asProject = prj,
      asCachedResolved = mempty
    }

run ::
  Project Annotation ->
  ActionM a ->
  Either (Error Annotation) (Project Annotation, [ActionOutcome], a)
run project action =
  let ((result, outcomes), ActionState newProject _) =
        runState (runWriterT (runExceptT action)) (emptyState project)
   in (,,) newProject outcomes <$> result

getProject :: ActionM (Project Annotation)
getProject = gets asProject

setProject :: Project Annotation -> ActionM ()
setProject prj =
  modify (\s -> s {asProject = prj})

appendProject :: Project Annotation -> ActionM ()
appendProject prj =
  modify (\s -> s {asProject = asProject s <> prj})

appendMessage :: Text -> ActionM ()
appendMessage =
  tell . pure . NewMessage

appendDocMessage :: Doc ann -> ActionM ()
appendDocMessage = appendMessage . renderWithWidth 50

-- | cache a resolved expression
appendResolvedExpression :: ExprHash -> ResolvedExpression Annotation -> ActionM ()
appendResolvedExpression exprHash re =
  modify
    ( \s ->
        s
          { asCachedResolved =
              asCachedResolved s <> M.singleton exprHash re
          }
    )

getResolvedExpressions :: ActionM (Map ExprHash (ResolvedExpression Annotation))
getResolvedExpressions =
  gets asCachedResolved

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
