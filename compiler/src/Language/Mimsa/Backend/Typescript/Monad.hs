{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Backend.Typescript.Monad
  ( TypescriptM,
    TSCodegenState (..),
    TSStateStack,
    TSReaderState (..),
    runTypescriptM,
    getState,
    findTypeName,
    typeNameIsImport,
    unusedGenerics,
    addGenerics,
    addDataType,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

type TypescriptM =
  ExceptT
    (BackendError MonoType)
    (WriterT [TSDataType] (ReaderT TSReaderState (State TSStateStack)))

newtype TSReaderState = TSReaderState
  { tsConstructorTypes :: Map TyCon TyCon -- Just -> Maybe, Nothing -> Maybe etc
  }

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

-- | is this type in our reader context (and thus is it an import, and should
-- be use it with a namespace, ie, Maybe.Maybe?
typeNameIsImport :: (MonadReader TSReaderState m) => TyCon -> m Bool
typeNameIsImport tyCon = do
  consType <- asks tsConstructorTypes
  let typeNames = S.fromList (M.elems consType)
  pure (S.member tyCon typeNames)

-- given 'Just', (hopefully) return 'Maybe'
findTypeName :: (MonadReader TSReaderState m) => TyCon -> m (Maybe TyCon)
findTypeName tyCon = do
  consType <- asks tsConstructorTypes
  case M.lookup tyCon consType of
    Just typeName -> pure (Just typeName)
    Nothing -> pure Nothing

initialStack :: TSStateStack
initialStack =
  NE.singleton $
    TSCodegenState mempty mempty

runTypescriptM ::
  TSReaderState ->
  TypescriptM a ->
  Either (BackendError MonoType) (a, [TSDataType])
runTypescriptM readerState computation =
  case evalState (runReaderT (runWriterT (runExceptT computation)) readerState) initialStack of
    (Right a, dts) -> pure (a, dts)
    (Left e, _) -> throwError e