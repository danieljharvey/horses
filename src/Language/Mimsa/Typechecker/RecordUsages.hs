{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Typechecker.RecordUsages
  ( getRecordUsages,
    getSubstitutionsForRecordUsages,
    CombineMap (..),
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.ExprUtils
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

newtype CombineMap k v = CombineMap {getCombineMap :: Map k v}
  deriving (Eq, Show)

instance (Ord k, Semigroup v) => Semigroup (CombineMap k v) where
  (CombineMap a) <> (CombineMap b) = CombineMap (M.unionWith (<>) a b)

instance (Ord k, Semigroup v) => Monoid (CombineMap k v) where
  mempty = CombineMap mempty

seqTuple :: (a, Maybe b) -> Maybe (a, b)
seqTuple (a, mB) = case mB of
  Just b -> Just (a, b)
  Nothing -> Nothing

getSubstitutionsForRecordUsages :: Expr Variable ann -> TcMonad Substitutions
getSubstitutionsForRecordUsages expr = do
  let records = M.toList $ getCombineMap $ getRecordUsages expr
      toSubst (k, v) = (,) (variableToTypeIdentifier k) <$> toEmptyType v
  maybeSubst <- (fmap . fmap) seqTuple (traverse toSubst records)
  let mappy = M.fromList $ catMaybes maybeSubst
  pure (Substitutions mappy)

toEmptyType :: Set Name -> TcMonad (Maybe MonoType)
toEmptyType vars =
  if S.null vars
    then pure Nothing
    else Just . MTRecord mempty <$> items
  where
    items :: TcMonad (Map Name MonoType)
    items =
      M.fromList
        <$> traverse item (S.toList vars)
    item k =
      (,) k <$> getUnknown mempty

getRecordUsages ::
  Expr Variable ann ->
  CombineMap Variable (Set Name)
getRecordUsages = withMonoid getRecordUsages'
  where
    getRecordUsages' (MyLambda _ binder expr) =
      (True, CombineMap (M.singleton binder mempty) <> snd (getRecordUsages' expr))
    getRecordUsages' (MyRecordAccess _ expr name) =
      case getVariable expr of
        Just var -> (True, CombineMap $ M.singleton var (S.singleton name))
        Nothing -> (True, mempty)
    getRecordUsages' _ = (True, mempty)

getVariable :: Expr Variable ann -> Maybe Variable
getVariable (MyVar _ a) = Just a
getVariable _ = Nothing
