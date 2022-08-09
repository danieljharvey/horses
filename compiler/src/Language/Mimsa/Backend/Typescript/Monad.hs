{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Backend.Typescript.Monad
  ( TypescriptM,
    TSCodegenState (..),
    TSStateStack,
    TSReaderState (..),
    runTypescriptM,
    getState,
    addInfix,
    findInfix,
    findTypeName,
    typeNameIsImport,
    unusedGenerics,
    addGenerics,
    addDataType,
    addImport,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Either
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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
    (WriterT [TSWriterItem] (ReaderT TSReaderState (State TSStateStack)))

data TSReaderState = TSReaderState
  { tsConstructorTypes :: Map TyCon TypeName, -- Just -> Maybe, Nothing -> Maybe etc
    tsInfixNames :: Map InfixOp TSName -- infix operators from imports
  }

type TSWriterItem =
  Either TSDataType TSImport

-- | we keep the datatypes in both the writer and the state
-- because we want both a sum of all datatypes, and a stack where they are
-- brought into and out of scope
data TSCodegenState = TSCodegenState
  { csGenerics :: Set TSGeneric,
    csDataTypes :: [DataType],
    csInfix :: Map InfixOp TSExpr
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
    MonadWriter [TSWriterItem] m
  ) =>
  DataType ->
  TSDataType ->
  m ()
addDataType dt tsDt = do
  tell [Left tsDt]
  modifyState
    ( \codegenState ->
        codegenState
          { csDataTypes = csDataTypes codegenState <> [dt]
          }
    )

-- | add a datatype to both the Writer and current stack
addImport ::
  ( MonadWriter [TSWriterItem] m
  ) =>
  TSImport ->
  m ()
addImport tsImport =
  tell [Right tsImport]

-- | define an infix operator, binding it to some 2-arity function
addInfix :: (MonadState TSStateStack m) => InfixOp -> TSExpr -> m ()
addInfix op expr =
  modifyState
    ( \codegenState ->
        codegenState
          { csInfix =
              csInfix codegenState
                <> M.singleton op expr
          }
    )

-- | is this type in our reader context (and thus is it an import, and should
-- be use it with a namespace, ie, Maybe.Maybe?
typeNameIsImport :: (MonadReader TSReaderState m) => TypeName -> m Bool
typeNameIsImport typeName = do
  consType <- asks tsConstructorTypes
  let typeNames = S.fromList (M.elems consType)
  pure (S.member typeName typeNames)

-- given 'Just', (hopefully) return 'Maybe'
findTypeName :: (MonadReader TSReaderState m) => TyCon -> m (Maybe TypeName)
findTypeName tyCon = do
  consType <- asks tsConstructorTypes
  case M.lookup tyCon consType of
    Just typeName -> pure (Just typeName)
    Nothing -> pure Nothing

-- | lookup an infix op firstly in scope then in imports
-- later when we bin off scoped infix defintions we'll only
-- look in the reader state
findInfix ::
  ( MonadReader TSReaderState m,
    MonadState TSStateStack m,
    MonadError (BackendError MonoType) m
  ) =>
  InfixOp ->
  m TSExpr
findInfix op = do
  tsState <- getState
  case M.lookup op (csInfix tsState) of
    Just expr -> pure expr
    Nothing -> do
      infixMap <- asks tsInfixNames
      case M.lookup op infixMap of
        Just name -> pure (TSVar name)
        Nothing -> throwError (CustomOperatorNotFound op)

runTypescriptM ::
  TSReaderState ->
  TSCodegenState ->
  TypescriptM a ->
  Either (BackendError MonoType) (a, [TSDataType], [TSImport])
runTypescriptM readerState startState computation =
  case evalState
    ( runReaderT
        (runWriterT (runExceptT computation))
        readerState
    )
    (NE.singleton startState) of
    (Right a, writerOutput) ->
      let (dts, imports) = partitionEithers writerOutput
       in pure (a, dts, imports)
    (Left e, _) -> throwError e
