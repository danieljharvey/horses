{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Backend.IR.FromExpr.Pattern
  ( predicatesFromPattern,
    destructurePattern,
  )
where

import Control.Monad.Identity
import Control.Monad.State
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Smol.Backend.IR.FromExpr.Helpers
import Smol.Backend.IR.FromExpr.Types
import Smol.Backend.Types.GetPath
import Smol.Backend.Types.PatternPredicate
import Smol.Core.Helpers
import qualified Smol.Core.Types as Smol

-- | given a pattern, pull out all the vars we're interested in and how to get
-- to them
destructurePattern ::
  ( MonadState s m,
    Ord p
  ) =>
  (Smol.Identifier -> p) ->
  Smol.Pattern Identity (Smol.Type Identity ann) ->
  m (Map p GetPath)
destructurePattern fromIdentifier =
  destructInner []
  where
    destructInner path (Smol.PTuple _ pHead pTail) =
      let pats = [pHead] <> NE.toList pTail
       in mconcat <$> traverseInd (\pat i -> destructInner (path <> [i]) pat) pats
    destructInner _ (Smol.PWildcard _) = pure mempty
    destructInner _ (Smol.PLiteral _ _) = pure mempty
    destructInner path (Smol.PArray _ as spread) = do
      let pathSoFar = path <> [1] -- arrays are length-indexed so actually (lengthInt, [items])
      let spreadPath = case spread of
            Smol.NoSpread -> mempty
            Smol.SpreadWildcard _ -> mempty
            Smol.SpreadValue _ ident ->
              M.singleton
                (fromIdentifier (runIdentity ident))
                (GetPath path (GetArrayTail (fromIntegral $ length as)))
      mappend spreadPath . mconcat <$> traverseInd (\pat i -> destructInner (pathSoFar <> [i]) pat) as
    destructInner path (Smol.PConstructor _ _ pArgs) = do
      -- get DataTypeInMemory from `ty`
      -- use it to work out where to reach into Struct to find data
      -- then return path
      mconcat <$> traverseInd (\pat i -> destructInner (path <> [i + 1]) pat) pArgs
    destructInner path (Smol.PVar _ ident) =
      pure $ M.singleton (fromIdentifier (runIdentity ident)) (GetPath path GetValue)

predicatesFromPattern ::
  ( MonadState (FromExprState ann) m,
    Show ann
  ) =>
  (Smol.Prim -> m p) ->
  Smol.Pattern Identity (Smol.Type Identity ann) ->
  m [PatternPredicate p]
predicatesFromPattern fromPrim =
  predicatesInner []
  where
    predicatesInner _ (Smol.PWildcard _) = pure mempty
    predicatesInner _ (Smol.PVar _ _) = pure mempty
    predicatesInner path (Smol.PArray _ pats spread) = do
      llPrim <-
        fromPrim
          ( Smol.PInt
              (fromIntegral $ length pats)
          )
      let spreadPred = case spread of
            Smol.NoSpread ->
              [ PathEquals
                  (GetPath (path <> [0]) GetValue)
                  llPrim
              ]
            _ -> []
      mappend spreadPred . mconcat
        <$> traverseInd
          ( \pat i ->
              predicatesInner (path <> [1, i]) pat
          )
          pats
    predicatesInner path (Smol.PLiteral _ prim) = do
      llPrim <- fromPrim prim
      pure [PathEquals (GetPath path GetValue) llPrim]
    predicatesInner path (Smol.PTuple _ pHead pTail) =
      let pats = [pHead] <> NE.toList pTail
       in mconcat <$> traverseInd (\pat i -> predicatesInner (path <> [i]) pat) pats
    predicatesInner path (Smol.PConstructor ty constructor pArgs) = do
      (_typeName, tyArgs) <- flattenConstructorType ty

      case tyArgs of
        -- if no args, it's a primitive
        [] -> do
          prim <- primFromConstructor (runIdentity constructor)
          llPrim <- fromPrim prim
          pure [PathEquals (GetPath path GetValue) llPrim]
        _ -> do
          -- if there's args it's a struct
          prim <- primFromConstructor (runIdentity constructor)
          llPrim <- fromPrim prim
          let constructorPath = PathEquals (GetPath (path <> [0]) GetValue) llPrim

          -- work out predicates for rest of items
          predRest <- mconcat <$> traverseInd (\pat i -> predicatesInner (path <> [i + 1]) pat) pArgs
          pure $ [constructorPath] <> predRest
