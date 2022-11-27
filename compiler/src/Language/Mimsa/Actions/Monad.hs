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
    bindModuleInProject,
    messagesFromOutcomes,
    modulesFromOutcomes,
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Language.Mimsa.Actions.Types
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker
import Prettyprinter

emptyState :: Project Annotation -> ActionState
emptyState prj =
  ActionState
    { asProject = prj,
      asCachedResolved = mempty,
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
