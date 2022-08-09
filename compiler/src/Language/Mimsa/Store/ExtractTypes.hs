module Language.Mimsa.Store.ExtractTypes
  ( extractTypes,
    extractTypeDecl,
    extractDataTypes,
    extractNamedTypeVars,
    extractTypenames,
  )
where

import Data.Coerce
import qualified Data.Map.Strict as M
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
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

-- this works out which external constructors have been used in a given expression
-- therefore, we must remove any which are declared in the expression itself
extractTypes :: Expr Name ann -> Set TyCon
extractTypes = filterBuiltIns . extractTypes_

extractTypes_ :: Expr Name ann -> Set TyCon
extractTypes_ MyVar {} = mempty
extractTypes_ (MyAnnotation _ _ expr) = extractTypes_ expr
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
    (extractTypes_ a)
    (extractLocalTypeDeclarations dt)
extractTypes_ (MyConstructor _ _ t) = S.singleton t
extractTypes_ (MyTypedHole _ _) = mempty
extractTypes_ (MyDefineInfix _ _ a b) =
  extractTypes_ a <> extractTypes_ b
extractTypes_ (MyPatternMatch _ expr patterns) =
  extractTypes_ expr
    <> mconcat (extractTypes_ . snd <$> patterns)
    <> mconcat (extractFromPattern . fst <$> patterns)

extractFromPattern :: Pattern var ann -> Set TyCon
extractFromPattern (PConstructor _ _ tyCon args) =
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
filterBuiltIns = S.filter (\c -> not $ M.member (coerce c) builtInTypes)

-- get all the names of constructors (type and data) declared in the datatype
extractLocalTypeDeclarations :: DataType -> Set TyCon
extractLocalTypeDeclarations (DataType cName _ cons) =
  S.singleton (coerce cName) -- TODO: this is the type name and should not be included but also YOLO
    <> mconcat (S.singleton . fst <$> M.toList cons)

-----------

extractTypeDecl :: Expr var ann -> Set TyCon
extractTypeDecl = withDataTypes extractLocalTypeDeclarations

extractDataTypes :: Expr var ann -> Set DataType
extractDataTypes = withDataTypes S.singleton

withDataTypes :: (Monoid b) => (DataType -> b) -> Expr var ann -> b
withDataTypes _ MyVar {} = mempty
withDataTypes f (MyAnnotation _ _ expr) = withDataTypes f expr
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
withDataTypes _ MyConstructor {} = mempty
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

extractTypenames :: Type ann -> Set TypeName
extractTypenames (MTConstructor _ _ typeName) =
  S.singleton typeName
extractTypenames other = withMonoid extractTypenames other

-----

extractNamedTypeVars :: Type ann -> Set TyVar
extractNamedTypeVars (MTVar _ (TVName tv)) = S.singleton tv
extractNamedTypeVars (MTVar _ (TVScopedVar _ name)) = S.singleton (coerce name)
extractNamedTypeVars other = withMonoid extractNamedTypeVars other
