{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Actions.Monad
  ( run,
    getProject,
    appendProject,
    appendMessage,
    appendWriteFile,
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
import qualified Data.Text as T
import Language.Mimsa.Project
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

newtype SavePath = SavePath Text
  deriving newtype (Eq, Ord)

instance Show SavePath where
  show (SavePath s) = T.unpack s

newtype SaveContents = SaveContents Text
  deriving newtype (Eq, Ord, Show)

newtype SaveFilename = SaveFilename Text
  deriving newtype (Eq, Ord)

instance Show SaveFilename where
  show (SaveFilename s) = T.unpack s

data ActionOutcome
  = NewMessage Text
  | NewStoreExpression (StoreExpression Annotation)
  | NewWriteFile SavePath SaveFilename SaveContents
  deriving (Eq, Ord, Show)

type ActionM =
  ExceptT
    (Error Annotation)
    (WriterT [ActionOutcome] (State (Project Annotation)))

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

appendWriteFile ::
  SavePath ->
  SaveFilename ->
  SaveContents ->
  ActionM ()
appendWriteFile savePath filename content =
  tell $ pure $ NewWriteFile savePath filename content

appendStoreExpression :: StoreExpression Annotation -> ActionM ()
appendStoreExpression =
  tell . pure . NewStoreExpression

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
  let newProject =
        fromItem name storeExpr (getStoreExpressionHash storeExpr)
  appendStoreExpression storeExpr
  appendProject newProject

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
