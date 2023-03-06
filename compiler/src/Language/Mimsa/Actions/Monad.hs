{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Actions.Monad
  ( run,
    getProject,
    appendProject,
    appendMessage,
    appendDocMessage,
    appendWriteFile,
    cacheTypecheckedStoreExpression,
    setProject,
    appendStoreExpression,
    bindModuleInProject,
    messagesFromOutcomes,
    modulesFromOutcomes,
    storeExpressionsFromOutcomes,
    writeFilesFromOutcomes,
    getCachedTypecheckedStoreExpressions,
    ActionM,
    SavePath (..),
    SaveContents (..),
    SaveFilename (..),
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Language.Mimsa.Actions.Types
import Language.Mimsa.Core
import Language.Mimsa.Project
import Language.Mimsa.Store
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Prettyprinter

emptyState :: Project Annotation -> ActionState
emptyState prj =
  ActionState
    { asProject = prj,
      asCachedTypechecked = mempty,
      asActionOutcomes = mempty
    }

run ::
  Project Annotation ->
  ActionM a ->
  Either (Error Annotation) (Project Annotation, [ActionOutcome], a)
run project action =
  let (result, ActionState newProject _ outcomes) =
        runState (runExceptT (runActionM action)) (emptyState project)
   in (,,) newProject outcomes <$> result

getProject :: ActionM (Project Annotation)
getProject = gets asProject

setProject :: Project Annotation -> ActionM ()
setProject prj =
  modify (\s -> s {asProject = prj})

appendProject :: Project Annotation -> ActionM ()
appendProject prj =
  modify (\s -> s {asProject = asProject s <> prj})

appendActionOutcome :: ActionOutcome -> ActionM ()
appendActionOutcome ao =
  modify (\s -> s {asActionOutcomes = asActionOutcomes s <> [ao]})

appendMessage :: Text -> ActionM ()
appendMessage =
  appendActionOutcome . NewMessage

appendDocMessage :: Doc ann -> ActionM ()
appendDocMessage = appendMessage . renderWithWidth 50

-- | cache a resolved expression
cacheTypecheckedStoreExpression :: ExprHash -> StoreExpression (Type Annotation) -> ActionM ()
cacheTypecheckedStoreExpression exprHash re =
  modify
    ( \s ->
        s
          { asCachedTypechecked =
              asCachedTypechecked s <> M.singleton exprHash re
          }
    )

getCachedTypecheckedStoreExpressions :: ActionM (Map ExprHash (StoreExpression (Type Annotation)))
getCachedTypecheckedStoreExpressions =
  gets asCachedTypechecked

appendWriteFile ::
  SavePath ->
  SaveFilename ->
  SaveContents ->
  ActionM ()
appendWriteFile savePath filename content =
  appendActionOutcome $ NewWriteFile savePath filename content

appendStoreExpression :: StoreExpression Annotation -> ActionM ()
appendStoreExpression se = do
  let newProject = fromStoreExpression se (getStoreExpressionHash se)
  appendActionOutcome (NewStoreExpression se)
  appendProject newProject

appendModule :: Module Annotation -> ActionM ()
appendModule =
  appendActionOutcome . NewModule

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

modulesFromOutcomes :: [ActionOutcome] -> Set (Module Annotation)
modulesFromOutcomes =
  S.fromList
    . foldMap
      ( \case
          NewModule mod' -> pure mod'
          _ -> mempty
      )

-- add binding for module and add it to store
bindModuleInProject ::
  Module (Type Annotation) ->
  ModuleName ->
  ActionM ()
bindModuleInProject typecheckedModule modName = do
  let untypedModule = getAnnotationForType <$> typecheckedModule
  appendModule untypedModule
  appendProject
    ( fromModule modName untypedModule
    )
