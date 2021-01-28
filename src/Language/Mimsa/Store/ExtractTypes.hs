module Language.Mimsa.Store.ExtractTypes
  ( extractTypes,
    extractTypeDecl,
    extractDataTypes,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Typechecker.DataTypes (builtInTypes)
import Language.Mimsa.Types.AST
  ( DataType (DataType),
    Expr (..),
    Field (..),
  )
import Language.Mimsa.Types.Identifiers (Name, TyCon)

-- this works out which external types have been used in a given expression
-- therefore, we must remove any which are declared in the expression itself
extractTypes :: Expr Name ann -> Set TyCon
extractTypes = filterBuiltIns . extractTypes_

extractTypes_ :: Expr Name ann -> Set TyCon
extractTypes_ (MyVar _ _) = mempty
extractTypes_ (MyIf _ a b c) = extractTypes_ a <> extractTypes_ b <> extractTypes_ c
extractTypes_ (MyLet _ _ a b) = extractTypes_ a <> extractTypes_ b
extractTypes_ (MyInfix _ _ a b) = extractTypes_ a <> extractTypes_ b
extractTypes_ (MyLambda _ _ a) = extractTypes_ a
extractTypes_ (MyApp _ a b) = extractTypes_ a <> extractTypes_ b
extractTypes_ (MyLiteral _ _) = mempty
extractTypes_ (MyLetPair _ _ _ a b) =
  extractTypes_ a <> extractTypes_ b
extractTypes_ (MyPair _ a b) = extractTypes_ a <> extractTypes_ b
extractTypes_ (MyRecord _ map') = foldMap extractTypes_ map'
extractTypes_ (MyRecordAccess _ a _) = extractTypes_ a
extractTypes_ (MyData _ dt a) =
  S.difference
    (extractConstructors dt <> extractTypes_ a)
    (extractLocalTypeDeclarations dt)
extractTypes_ (MyConstructor _ t) = S.singleton t
extractTypes_ (MyConsApp _ a b) = extractTypes_ a <> extractTypes_ b
extractTypes_ (MyCaseMatch _ sum' matches catchAll) =
  extractTypes_ sum'
    <> mconcat (extractTypes_ . snd <$> NE.toList matches)
    <> mconcat (S.singleton . fst <$> NE.toList matches)
    <> maybe mempty extractTypes catchAll
extractTypes_ (MyTypedHole _ _) = mempty
extractTypes_ (MyDefineInfix _ _ a b) =
  extractTypes_ a <> extractTypes_ b

filterBuiltIns :: Set TyCon -> Set TyCon
filterBuiltIns = S.filter (\c -> not $ M.member c builtInTypes)

-- get all the constructors mentioned in the datatype
extractConstructors :: DataType -> Set TyCon
extractConstructors (DataType _ _ cons) = mconcat (extractFromCons . snd <$> M.toList cons)
  where
    extractFromCons as = mconcat (extractFromCon <$> as)
    extractFromCon (VarName _) = mempty
    extractFromCon (ConsName name as) = S.singleton name <> mconcat (extractFromCon <$> as)
    extractFromCon (TNFunc a b) = extractFromCon a <> extractFromCon b

-- get all the names of constructors (type and data) declared in the datatype
extractLocalTypeDeclarations :: DataType -> Set TyCon
extractLocalTypeDeclarations (DataType cName _ cons) =
  S.singleton cName
    <> mconcat (S.singleton . fst <$> M.toList cons)

-----------

extractTypeDecl :: Expr var ann -> Set TyCon
extractTypeDecl = withDataTypes extractLocalTypeDeclarations

extractDataTypes :: Expr var ann -> Set DataType
extractDataTypes = withDataTypes S.singleton

withDataTypes :: (Monoid b) => (DataType -> b) -> Expr var ann -> b
withDataTypes _ (MyVar _ _) = mempty
withDataTypes f (MyIf _ a b c) = withDataTypes f a <> withDataTypes f b <> withDataTypes f c
withDataTypes f (MyLet _ _ a b) = withDataTypes f a <> withDataTypes f b
withDataTypes f (MyInfix _ _ a b) = withDataTypes f a <> withDataTypes f b
withDataTypes f (MyLambda _ _ a) = withDataTypes f a
withDataTypes f (MyApp _ a b) = withDataTypes f a <> withDataTypes f b
withDataTypes _ (MyLiteral _ _) = mempty
withDataTypes f (MyLetPair _ _ _ a b) =
  withDataTypes f a <> withDataTypes f b
withDataTypes f (MyPair _ a b) = withDataTypes f a <> withDataTypes f b
withDataTypes f (MyRecord _ map') = foldMap (withDataTypes f) map'
withDataTypes f (MyRecordAccess _ a _) = withDataTypes f a
withDataTypes f (MyData _ dt a) =
  withDataTypes f a
    <> f dt
withDataTypes _ (MyConstructor _ _) = mempty
withDataTypes f (MyConsApp _ a b) = withDataTypes f a <> withDataTypes f b
withDataTypes f (MyCaseMatch _ sum' matches catchAll) =
  withDataTypes f sum'
    <> mconcat (withDataTypes f . snd <$> NE.toList matches)
    <> maybe mempty (withDataTypes f) catchAll
withDataTypes _ (MyTypedHole _ _) = mempty
withDataTypes f (MyDefineInfix _ _ a b) = withDataTypes f a <> withDataTypes f b
