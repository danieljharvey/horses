module Language.Mimsa.Actions.Monad
  ( run,
    getProject,
    appendProject,
    appendMessage,
    setProject,
    addStoreExpression,
    bindStoreExpression,
    ActionState (..),
    ActionM,
  )
where

import Control.Monad.Except
import Control.Monad.State
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

data ActionState = ActionState
  { asProject :: Project Annotation,
    asStoreExpressions :: Set (StoreExpression Annotation),
    asMessages :: [Text]
  }
  deriving (Eq, Ord, Show)

type ActionM = ExceptT (Error Annotation) (State ActionState)

run :: Project Annotation -> ActionM a -> Either (Error Annotation) (ActionState, a)
run project action =
  let startState =
        ActionState project mempty mempty
      (result, newState) = runState (runExceptT action) startState
   in (,) newState <$> result

getProject :: ActionM (Project Annotation)
getProject = gets asProject

setProject :: Project Annotation -> ActionM ()
setProject prj =
  modify (\as -> as {asProject = prj})

appendProject :: Project Annotation -> ActionM ()
appendProject prj =
  modify (\as -> as {asProject = asProject as <> prj})

appendMessage :: Text -> ActionM ()
appendMessage txt =
  modify (\as -> as {asMessages = asMessages as <> [txt]})

addStoreExpression :: StoreExpression Annotation -> ActionM ()
addStoreExpression se =
  modify
    ( \as ->
        as
          { asStoreExpressions =
              S.singleton se <> asStoreExpressions as
          }
    )

-- add binding for expression and add it to store
bindStoreExpression ::
  StoreExpression Annotation ->
  Name ->
  ActionM ()
bindStoreExpression storeExpr name = do
  let newProject =
        fromItem name storeExpr (getStoreExpressionHash storeExpr)
  addStoreExpression storeExpr
  appendProject newProject
