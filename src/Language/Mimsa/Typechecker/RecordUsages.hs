{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Typechecker.RecordUsages
  ( getRecordUsages,
    getSubstitutionsForRecordUsages,
    CombineMap (..),
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.Set (Set)
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
      toSubst (k, v) = (,) k <$> toEmptyType v
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
getRecordUsages (MyLiteral _ _) = mempty
getRecordUsages (MyVar _ _) = mempty
getRecordUsages (MyLet _ binder bindExpr' inExpr) =
  CombineMap (M.singleton binder mempty) <> getRecordUsages bindExpr'
    <> getRecordUsages inExpr
getRecordUsages (MyLetPair _ binderA binderB bindExpr' inExpr) =
  CombineMap (M.fromList [(binderA, mempty), (binderB, mempty)])
    <> getRecordUsages bindExpr'
    <> getRecordUsages inExpr
getRecordUsages (MyInfix _ _ a b) =
  getRecordUsages a <> getRecordUsages b
getRecordUsages (MyLambda _ binder expr) =
  CombineMap (M.singleton binder mempty) <> getRecordUsages expr
getRecordUsages (MyApp _ func arg) = getRecordUsages func <> getRecordUsages arg
getRecordUsages (MyIf _ matchExpr thenExpr elseExpr) =
  getRecordUsages matchExpr
    <> getRecordUsages thenExpr
    <> getRecordUsages elseExpr
getRecordUsages (MyPair _ a b) =
  getRecordUsages a <> getRecordUsages b
getRecordUsages (MyRecord _ items) = mconcat $ snd <$> M.toList (getRecordUsages <$> items)
getRecordUsages (MyRecordAccess _ expr name) =
  case getVariable expr of
    Just var -> CombineMap $ M.singleton var (S.singleton name)
    Nothing -> mempty
getRecordUsages (MyData _ _ expr) = getRecordUsages expr
getRecordUsages (MyConstructor _ _) = mempty
getRecordUsages (MyConsApp _ func arg) =
  getRecordUsages func <> getRecordUsages arg
getRecordUsages (MyCaseMatch _ matchExpr caseExprs catchExpr) =
  getRecordUsages matchExpr
    <> mconcat
      ( NE.toList
          (getRecordUsages <$> (snd <$> caseExprs))
      )
    <> maybe mempty getRecordUsages catchExpr

getVariable :: Expr Variable ann -> Maybe Variable
getVariable (MyVar _ a) = Just a
getVariable _ = Nothing
