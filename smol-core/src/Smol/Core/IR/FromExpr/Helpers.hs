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

import Control.Monad.Identity
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Records (HasField (..))
import Smol.Core.Helpers
import Smol.Core.IR.FromExpr.Types
import qualified Smol.Core.Typecheck.Shared as TC
import qualified Smol.Core.Types as Smol

flattenConstructorType ::
  ( Monad m,
    Show ann,
    Show (dep Smol.Identifier),
    Show (dep Smol.TypeName)
  ) =>
  Smol.Type dep ann ->
  m (dep Smol.TypeName, [Smol.Type dep ann])
flattenConstructorType ty = do
  let result = TC.flattenConstructorType ty
  pure (fromRight result)

-- | lookup constructor, get number for it and expected number of args
-- we'll use this to create datatype etc
primFromConstructor ::
  ( MonadState (FromExprState ann) m
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
  ( MonadState (FromExprState ann) m
  ) =>
  Smol.Constructor ->
  m (Smol.DataType Identity ann)
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
  ( MonadState (FromExprState ann) m
  ) =>
  Identity Smol.TypeName ->
  m (Smol.DataType Identity ann)
lookupTypeName tn = do
  maybeDt <- gets (M.lookup tn . getField @"dataTypes")
  case maybeDt of
    Just dt -> pure dt
    Nothing -> error $ "couldn't find datatype for " <> show tn

getConstructorNumber :: Smol.DataType Identity ann -> Smol.Constructor -> Integer
getConstructorNumber (Smol.DataType _ _ constructors) constructor =
  case M.lookup constructor (mapToNumbered constructors) of
    Just i -> i
    Nothing -> error "blah"
