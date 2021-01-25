{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Actions.Monad
  ( run,
    getProject,
    appendProject,
    appendMessage,
    setProject,
    appendStoreExpression,
    bindStoreExpression,
    messagesFromOutcomes,
    storeExpressionsFromOutcomes,
    ActionM,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Language.Mimsa.Project
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

data ActionOutcome
  = NewMessage Text
  | NewStoreExpression (StoreExpression Annotation)
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
