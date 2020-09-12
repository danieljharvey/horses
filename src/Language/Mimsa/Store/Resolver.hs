{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Store.Resolver
  ( extractVars,
    createStoreExpression,
    createTypeStoreExpression,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
--
--
-- this takes the expression, works out what it needs from it's environment
-- and wraps that up
-- this would be a good place for a simplifying step in future
-- by replacing all variables internally with a1, a2 etc, we'll get less
-- repetition, ie \x -> x and \y -> y will be the same function and thus hash

import Language.Mimsa.Library
import Language.Mimsa.Types

-- important - we must not count variables brought in via lambdas, as those
-- aren't external deps

extractVars :: Expr Name -> Set Name
extractVars = filterBuiltIns . extractVars_

extractVars_ :: Expr Name -> Set Name
extractVars_ (MyVar a) = S.singleton a
extractVars_ (MyIf a b c) = extractVars_ a <> extractVars_ b <> extractVars_ c
extractVars_ (MyLet newVar a b) = S.delete newVar (extractVars_ a <> extractVars_ b)
extractVars_ (MyLambda newVar a) = S.delete newVar (extractVars_ a)
extractVars_ (MyApp a b) = extractVars_ a <> extractVars_ b
extractVars_ (MyLiteral _) = mempty
extractVars_ (MyLetPair newVarA newVarB a b) =
  S.delete
    newVarA
    (S.delete newVarB (extractVars_ a <> extractVars_ b))
extractVars_ (MyPair a b) = extractVars_ a <> extractVars_ b
extractVars_ (MyRecord map') = foldMap extractVars_ map'
extractVars_ (MyRecordAccess a _) = extractVars_ a
extractVars_ (MyData _ a) = extractVars_ a
extractVars_ (MyConstructor _) = mempty
extractVars_ (MyConsApp a b) = extractVars_ a <> extractVars_ b
extractVars_ (MyCaseMatch sum' matches catchAll) =
  extractVars sum'
    <> mconcat (extractVars . snd <$> NE.toList matches)
    <> maybe mempty extractVars catchAll

filterBuiltIns :: Set Name -> Set Name
filterBuiltIns = S.filter (not . isLibraryName)

findHashInBindings :: Bindings -> Name -> Either ResolverError ExprHash
findHashInBindings (Bindings bindings') name = case M.lookup name bindings' of
  Just a -> Right a
  _ -> Left $ MissingBinding name (Bindings bindings')

-- given an expression, and the current environment, create a
-- store expression that captures the hashes of the functions we'll need
createStoreExpression :: Bindings -> Expr Name -> Either ResolverError StoreExpression
createStoreExpression bindings' expr = do
  let findHash name =
        (,) name
          <$> findHashInBindings bindings' name
  hashes <- traverse findHash (S.toList . extractVars $ expr)
  Right (StoreExpression expr (Bindings (M.fromList hashes)) mempty)

-----------
--
findHashInTypeBindings :: TypeBindings -> Construct -> Either ResolverError ExprHash
findHashInTypeBindings (TypeBindings bindings') cName = case M.lookup cName bindings' of
  Just a -> Right a
  _ -> Left $ MissingType cName (TypeBindings bindings')

-- get all the constructors mentioned in the datatype
extractConstructors :: DataType -> Set Construct
extractConstructors (DataType _ _ cons) = mconcat (extractFromCons . snd <$> M.toList cons)
  where
    extractFromCons as = mconcat (extractFromCon <$> as)
    extractFromCon (VarName _) = mempty
    extractFromCon (ConsName name as) = S.singleton name <> mconcat (extractFromCon <$> as)

-- given a data type declaration, create a StoreExpression for it
createTypeStoreExpression :: TypeBindings -> DataType -> Either ResolverError StoreExpression
createTypeStoreExpression tBindings dt = do
  let findHash cName =
        (,) cName
          <$> findHashInTypeBindings tBindings cName
  hashes <- traverse findHash (S.toList . extractConstructors $ dt)
  let newTypeBindings = TypeBindings $ M.fromList hashes
  pure (StoreExpression (MyData dt (MyRecord mempty)) mempty newTypeBindings)
