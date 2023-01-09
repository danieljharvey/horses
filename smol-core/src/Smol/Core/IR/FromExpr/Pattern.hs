{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.IR.FromExpr.Pattern
  ( predicatesFromPattern,
    destructurePattern,
  )
where

import Control.Monad.State
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Records (HasField (..))
import Smol.Core.Helpers
import Smol.Core.IR.FromExpr.Helpers
import qualified Smol.Core.Types as Smol
import Smol.Core.Types.GetPath
import Smol.Core.Types.PatternPredicate

-- | given a pattern, pull out all the vars we're interested in and how to get
-- to them
destructurePattern ::
  ( MonadState s m,
    Ord p
  ) =>
  (Smol.Identifier -> p) ->
  Smol.Pattern (Smol.Type ann) ->
  m (Map p GetPath)
destructurePattern fromIdentifier =
  destructInner []
  where
    destructInner path (Smol.PTuple _ pHead pTail) =
      let pats = [pHead] <> NE.toList pTail
       in mconcat <$> traverseInd (\pat i -> destructInner (path <> [i]) pat) pats
    destructInner _ (Smol.PWildcard _) = pure mempty
    destructInner _ (Smol.PLiteral _ _) = pure mempty
    destructInner path (Smol.PConstructor _ _ pArgs) = do
      -- get DataTypeInMemory from `ty`
      -- use it to work out where to reach into Struct to find data
      -- then return path
      mconcat <$> traverseInd (\pat i -> destructInner (path <> [i + 1]) pat) pArgs
    destructInner path (Smol.PVar _ ident) =
      case NE.nonEmpty path of
        Just nePath -> pure $ M.singleton (fromIdentifier ident) (StructPath nePath)
        Nothing -> pure $ M.singleton (fromIdentifier ident) ValuePath

predicatesFromPattern ::
  ( MonadState s m,
    HasField "dataTypes" s (Map Smol.TypeName (Smol.DataType ann)),
    Show ann
  ) =>
  (Smol.Prim -> p) ->
  Smol.Pattern (Smol.Type ann) ->
  m [PatternPredicate p]
predicatesFromPattern fromPrim =
  predicatesInner []
  where
    predicatesInner _ (Smol.PWildcard _) = pure mempty
    predicatesInner _ (Smol.PVar _ _) = pure mempty
    predicatesInner path (Smol.PLiteral _ prim) =
      case NE.nonEmpty path of
        Just nePath ->
          pure [PathEquals (StructPath nePath) (fromPrim prim)]
        Nothing -> pure [PathEquals ValuePath (fromPrim prim)]
    predicatesInner path (Smol.PTuple _ pHead pTail) =
      let pats = [pHead] <> NE.toList pTail
       in mconcat <$> traverseInd (\pat i -> predicatesInner (path <> [i]) pat) pats
    predicatesInner path (Smol.PConstructor ty constructor pArgs) = do
      (_typeName, tyArgs) <- flattenConstructorType ty

      case tyArgs of
        -- if no args, it's a primitive
        [] -> do
          prim <- primFromConstructor constructor
          case NE.nonEmpty path of
            Just nePath -> pure [PathEquals (StructPath nePath) (fromPrim prim)]
            Nothing -> pure [PathEquals ValuePath (fromPrim prim)]
        _ -> do
          -- if there's args it's a struct
          prim <- primFromConstructor constructor
          let constructorPath = case NE.nonEmpty path of
                Just nePath -> PathEquals (StructPath $ nePath <> NE.singleton 0) (fromPrim prim)
                Nothing -> PathEquals (StructPath (NE.singleton 0)) (fromPrim prim)

          -- work out predicates for rest of items
          predRest <- mconcat <$> traverseInd (\pat i -> predicatesInner (path <> [i + 1]) pat) pArgs
          pure $ [constructorPath] <> predRest
