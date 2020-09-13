module Language.Mimsa.Store.ExtractTypes
  ( extractTypes,
    extractTypeDecl,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Typechecker.DataTypes (builtInTypes)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

-- this works out which external types have been used in a given expression
-- therefore, we must remove any which are declared in the expression itself
extractTypes :: Expr Name -> Set Construct
extractTypes = filterBuiltIns . extractTypes_

extractTypes_ :: Expr Name -> Set Construct
extractTypes_ (MyVar _) = mempty
extractTypes_ (MyIf a b c) = extractTypes_ a <> extractTypes_ b <> extractTypes_ c
extractTypes_ (MyLet _ a b) = extractTypes_ a <> extractTypes_ b
extractTypes_ (MyLambda _ a) = extractTypes_ a
extractTypes_ (MyApp a b) = extractTypes_ a <> extractTypes_ b
extractTypes_ (MyLiteral _) = mempty
extractTypes_ (MyLetPair _ _ a b) =
  extractTypes_ a <> extractTypes_ b
extractTypes_ (MyPair a b) = extractTypes_ a <> extractTypes_ b
extractTypes_ (MyRecord map') = foldMap extractTypes_ map'
extractTypes_ (MyRecordAccess a _) = extractTypes_ a
extractTypes_ (MyData dt a) =
  S.difference
    (extractConstructors dt <> extractTypes_ a)
    (extractLocalTypeDeclarations dt)
extractTypes_ (MyConstructor t) = S.singleton t
extractTypes_ (MyConsApp a b) = extractTypes_ a <> extractTypes_ b
extractTypes_ (MyCaseMatch sum' matches catchAll) =
  extractTypes_ sum'
    <> mconcat (extractTypes_ . snd <$> NE.toList matches)
    <> mconcat (S.singleton . fst <$> NE.toList matches)
    <> maybe mempty extractTypes catchAll

filterBuiltIns :: Set Construct -> Set Construct
filterBuiltIns = S.filter (\c -> not $ M.member c builtInTypes)

-- get all the constructors mentioned in the datatype
extractConstructors :: DataType -> Set Construct
extractConstructors (DataType _ _ cons) = mconcat (extractFromCons . snd <$> M.toList cons)
  where
    extractFromCons as = mconcat (extractFromCon <$> as)
    extractFromCon (VarName _) = mempty
    extractFromCon (ConsName name as) = S.singleton name <> mconcat (extractFromCon <$> as)

-- get all the names of constructors (type and data) declared in the datatype
extractLocalTypeDeclarations :: DataType -> Set Construct
extractLocalTypeDeclarations (DataType cName _ cons) =
  S.singleton cName
    <> mconcat (S.singleton . fst <$> M.toList cons)

----------

extractTypeDecl :: Expr a -> Set Construct
extractTypeDecl (MyVar _) = mempty
extractTypeDecl (MyIf a b c) = extractTypeDecl a <> extractTypeDecl b <> extractTypeDecl c
extractTypeDecl (MyLet _ a b) = extractTypeDecl a <> extractTypeDecl b
extractTypeDecl (MyLambda _ a) = extractTypeDecl a
extractTypeDecl (MyApp a b) = extractTypeDecl a <> extractTypeDecl b
extractTypeDecl (MyLiteral _) = mempty
extractTypeDecl (MyLetPair _ _ a b) =
  extractTypeDecl a <> extractTypeDecl b
extractTypeDecl (MyPair a b) = extractTypeDecl a <> extractTypeDecl b
extractTypeDecl (MyRecord map') = foldMap extractTypeDecl map'
extractTypeDecl (MyRecordAccess a _) = extractTypeDecl a
extractTypeDecl (MyData dt a) =
  extractTypeDecl a
    <> extractLocalTypeDeclarations dt
extractTypeDecl (MyConstructor _) = mempty
extractTypeDecl (MyConsApp a b) = extractTypeDecl a <> extractTypeDecl b
extractTypeDecl (MyCaseMatch sum' matches catchAll) =
  extractTypeDecl sum'
    <> mconcat (extractTypeDecl . snd <$> NE.toList matches)
    <> maybe mempty extractTypeDecl catchAll
