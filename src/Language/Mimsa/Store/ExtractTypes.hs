module Language.Mimsa.Store.ExtractTypes
  ( extractTypes,
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
