{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Backend.Typescript.Monad
  ( TypescriptM,
    TSCodegenState (..),
    TSStateStack,
    runTypescriptM,
    getState,
    unusedGenerics,
    addGenerics,
    addDataType,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Typechecker

type TypescriptM =
  ExceptT
    (BackendError MonoType)
    (WriterT [TSDataType] (State TSStateStack))

-- | we keep the datatypes in both the writer and the state
-- because we want both a sum of all datatypes, and a stack where they are
-- brought into and out of scope
data TSCodegenState = TSCodegenState
  { csGenerics :: Set TSGeneric,
    csDataTypes :: [DataType]
  }
  deriving stock (Eq, Ord, Show)

type TSStateStack = NE.NonEmpty TSCodegenState

-- | Modify the current stack entry (ie, head of NE list)
modifyState ::
  (MonadState TSStateStack m) =>
  (TSCodegenState -> TSCodegenState) ->
  m ()
modifyState f =
  modify
    ( \stack ->
        let newHead = f (NE.head stack)
         in newHead :| NE.tail stack
    )

-- | get current state entry
getState ::
  (MonadState TSStateStack m) =>
  m TSCodegenState
getState = gets NE.head

-- | add new generics to state
addGenerics ::
  (MonadState TSStateStack m) =>
  Set TSGeneric ->
  m ()
addGenerics generics =
  modifyState
    ( \codegenState ->
        codegenState
          { csGenerics =
              csGenerics codegenState <> generics
          }
    )

-- given some generics, return the ones we haven't already seen, then add them
-- to state
unusedGenerics ::
  (MonadState TSStateStack m) =>
  Set TSGeneric ->
  m (Set TSGeneric)
unusedGenerics new = do
  old <- getState
  let unused = S.difference new (csGenerics old)
  addGenerics new
  pure unused

-- | add a datatype to both the Writer and current stack
addDataType ::
  ( MonadState TSStateStack m,
    MonadWriter [TSDataType] m
  ) =>
  DataType ->
  TSDataType ->
  m ()
addDataType dt tsDt = do
  tell [tsDt]
  modifyState
    ( \codegenState ->
        codegenState
          { csDataTypes = csDataTypes codegenState <> [dt]
          }
    )

initialStack :: TSStateStack
initialStack =
  NE.singleton $
    TSCodegenState mempty mempty

runTypescriptM ::
  TypescriptM a ->
  Either (BackendError MonoType) (a, [TSDataType])
runTypescriptM computation =
  case evalState (runWriterT (runExceptT computation)) initialStack of
    (Right a, dts) -> pure (a, dts)
    (Left e, _) -> throwError e
