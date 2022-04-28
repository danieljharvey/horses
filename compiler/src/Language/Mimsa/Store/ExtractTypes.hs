module Language.Mimsa.Store.ExtractTypes
  ( extractTypes,
    extractTypeDecl,
    extractDataTypes,
    extractNamedTypeVars,
    extractTypenames,
  )
where

import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.TypeUtils
import Language.Mimsa.Typechecker.BuiltIns
import Language.Mimsa.Types.AST
  ( DataType (DataType),
    Expr (..),
    Identifier (..),
    Pattern (..),
  )
import Language.Mimsa.Types.Identifiers (Name, TyCon, TyVar, TypeIdentifier (..))
import Language.Mimsa.Types.Typechecker

-- this works out which external types have been used in a given expression
-- therefore, we must remove any which are declared in the expression itself
extractTypes :: Expr Name ann -> Set TyCon
extractTypes = filterBuiltIns . extractTypes_

extractTypes_ :: Expr Name ann -> Set TyCon
extractTypes_ (MyVar _ _) = mempty
extractTypes_ (MyAnnotation _ expr _) = extractTypes_ expr
extractTypes_ (MyIf _ a b c) = extractTypes_ a <> extractTypes_ b <> extractTypes_ c
extractTypes_ (MyLet _ ident a b) =
  extractFromIdentifier ident
    <> extractTypes_ a
    <> extractTypes_ b
extractTypes_ (MyInfix _ _ a b) = extractTypes_ a <> extractTypes_ b
extractTypes_ (MyLambda _ ident a) =
  extractFromIdentifier ident <> extractTypes_ a
extractTypes_ (MyApp _ a b) = extractTypes_ a <> extractTypes_ b
extractTypes_ (MyLiteral _ _) = mempty
extractTypes_ (MyLetPattern _ pat expr body) =
  extractFromPattern pat <> extractTypes_ expr <> extractTypes_ body
extractTypes_ (MyPair _ a b) = extractTypes_ a <> extractTypes_ b
extractTypes_ (MyRecord _ map') = foldMap extractTypes_ map'
extractTypes_ (MyRecordAccess _ a _) = extractTypes_ a
extractTypes_ (MyArray _ items) = foldMap extractTypes_ items
extractTypes_ (MyData _ dt a) =
  S.difference
    (extractConstructors dt <> extractTypes_ a)
    (extractLocalTypeDeclarations dt)
extractTypes_ (MyConstructor _ t) = S.singleton t
extractTypes_ (MyTypedHole _ _) = mempty
extractTypes_ (MyDefineInfix _ _ a b) =
  extractTypes_ a <> extractTypes_ b
extractTypes_ (MyPatternMatch _ expr patterns) =
  extractTypes_ expr
    <> mconcat (extractTypes_ . snd <$> patterns)
    <> mconcat (extractFromPattern . fst <$> patterns)

extractFromPattern :: Pattern var ann -> Set TyCon
extractFromPattern (PConstructor _ tyCon args) =
  S.singleton tyCon <> mconcat (extractFromPattern <$> args)
extractFromPattern (PPair _ a b) = extractFromPattern a <> extractFromPattern b
extractFromPattern (PRecord _ items) = mconcat $ extractFromPattern <$> M.elems items
extractFromPattern (PWildcard _) = mempty
extractFromPattern (PVar _ _) = mempty
extractFromPattern (PLit _ _) = mempty
extractFromPattern (PArray _ as _) = mconcat $ extractFromPattern <$> as
extractFromPattern PString {} = mempty

extractFromIdentifier :: Identifier var ann -> Set TyCon
extractFromIdentifier _ = mempty

filterBuiltIns :: Set TyCon -> Set TyCon
filterBuiltIns = S.filter (\c -> not $ M.member c builtInTypes)

-- get all the constructors mentioned in the datatype
extractConstructors :: DataType -> Set TyCon
extractConstructors (DataType _ _ cons) = mconcat (extractFromCons . snd <$> M.toList cons)
  where
    extractFromCons as = mconcat (extractFromCon <$> as)
    extractFromCon (MTVar _ _) = mempty
    extractFromCon (MTFunction _ a b) = extractFromCon a <> extractFromCon b
    extractFromCon (MTPair _ a b) = extractFromCon a <> extractFromCon b
    extractFromCon (MTArray _ a) = extractFromCon a
    extractFromCon (MTTypeApp _ a b) = extractFromCon a <> extractFromCon b
    extractFromCon MTPrim {} = mempty
    extractFromCon (MTConstructor _ name) = S.singleton name
    extractFromCon (MTRecord _ items) = mconcat (extractFromCon <$> M.elems items)
    extractFromCon (MTRecordRow _ items rest) =
      mconcat (extractFromCon <$> M.elems items) <> extractFromCon rest

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
withDataTypes f (MyAnnotation _ expr _) = withDataTypes f expr
withDataTypes f (MyIf _ a b c) = withDataTypes f a <> withDataTypes f b <> withDataTypes f c
withDataTypes f (MyLet _ _ a b) = withDataTypes f a <> withDataTypes f b
withDataTypes f (MyInfix _ _ a b) = withDataTypes f a <> withDataTypes f b
withDataTypes f (MyLambda _ _ a) = withDataTypes f a
withDataTypes f (MyApp _ a b) = withDataTypes f a <> withDataTypes f b
withDataTypes _ (MyLiteral _ _) = mempty
withDataTypes f (MyLetPattern _ _ a b) =
  withDataTypes f a <> withDataTypes f b
withDataTypes f (MyPair _ a b) = withDataTypes f a <> withDataTypes f b
withDataTypes f (MyRecord _ map') = foldMap (withDataTypes f) map'
withDataTypes f (MyRecordAccess _ a _) = withDataTypes f a
withDataTypes f (MyArray _ map') = foldMap (withDataTypes f) map'
withDataTypes f (MyData _ dt a) =
  withDataTypes f a
    <> f dt
withDataTypes _ (MyConstructor _ _) = mempty
withDataTypes _ (MyTypedHole _ _) = mempty
withDataTypes f (MyDefineInfix _ _ infixA a) =
  withDataTypes f infixA <> withDataTypes f a
withDataTypes f (MyPatternMatch _ expr patterns) =
  withDataTypes f expr
    <> mconcat (withDataTypes f . snd <$> patterns)
    <> mconcat (extractFrom f . fst <$> patterns)
  where
    extractFrom _pat = mempty

-----

extractTypenames :: Type ann -> Set TyCon
extractTypenames (MTConstructor _ tyCon) =
  S.singleton tyCon
extractTypenames other = withMonoid extractTypenames other

-----

extractNamedTypeVars :: Type ann -> Set TyVar
extractNamedTypeVars (MTVar _ (TVName _ tv)) = S.singleton tv
extractNamedTypeVars other = withMonoid extractNamedTypeVars other
