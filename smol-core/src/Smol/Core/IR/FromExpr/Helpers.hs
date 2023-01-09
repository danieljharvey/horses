{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smol.Core.IR.FromExpr.Helpers
  ( flattenConstructorType,
    primFromConstructor,
    lookupTypeName,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Records (HasField (..))
import Smol.Core.Helpers
import qualified Smol.Core.Typecheck.Shared as TC
import qualified Smol.Core.Types as Smol

flattenConstructorType ::
  (Monad m, Show ann) =>
  Smol.Type ann ->
  m (Smol.TypeName, [Smol.Type ann])
flattenConstructorType ty = do
  result <-
    runExceptT $ TC.flattenConstructorType ty
  pure (fromRight result)

-- | lookup constructor, get number for it and expected number of args
-- we'll use this to create datatype etc
primFromConstructor ::
  ( MonadState s m,
    HasField "dataTypes" s (Map Smol.TypeName (Smol.DataType ann))
  ) =>
  Smol.Constructor ->
  m Smol.Prim
primFromConstructor constructor = do
  dt <- lookupConstructor constructor
  let i = getConstructorNumber dt constructor
  pure (Smol.PInt i)

-- | lookup constructor, get number for it and expected number of args
-- we'll use this to create datatype etc
lookupConstructor ::
  ( MonadState s m,
    HasField "dataTypes" s (Map Smol.TypeName (Smol.DataType ann))
  ) =>
  Smol.Constructor ->
  m (Smol.DataType ann)
lookupConstructor constructor = do
  maybeDt <-
    gets
      ( mapFind
          ( \dt@(Smol.DataType _ _ constructors) ->
              (,) dt <$> M.lookup constructor constructors
          )
          . getField @"dataTypes"
      )
  case maybeDt of
    Just (dt, _) -> pure dt
    Nothing -> error "cant find, what the hell man"

lookupTypeName ::
  ( MonadState s m,
    HasField "dataTypes" s (Map Smol.TypeName (Smol.DataType ann))
  ) =>
  Smol.TypeName ->
  m (Smol.DataType ann)
lookupTypeName tn = do
  maybeDt <- gets (M.lookup tn . getField @"dataTypes")
  case maybeDt of
    Just dt -> pure dt
    Nothing -> error $ "couldn't find datatype for " <> show tn

getConstructorNumber :: Smol.DataType ann -> Smol.Constructor -> Integer
getConstructorNumber (Smol.DataType _ _ constructors) constructor =
  case M.lookup constructor (mapToNumbered constructors) of
    Just i -> i
    Nothing -> error "blah"
