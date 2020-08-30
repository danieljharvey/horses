{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Store.Resolver where

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
extractVars_ (MyCase sum' l r) = extractVars_ sum' <> extractVars_ l <> extractVars_ r
extractVars_ (MyLetPair newVarA newVarB a b) =
  S.delete
    newVarA
    (S.delete newVarB (extractVars_ a <> extractVars_ b))
extractVars_ (MyLetList newVarA newVarB a b) =
  S.delete
    newVarA
    (S.delete newVarB (extractVars_ a <> extractVars_ b))
extractVars_ (MyPair a b) = extractVars_ a <> extractVars_ b
extractVars_ (MySum _ a) = extractVars_ a
extractVars_ (MyList as) = foldMap extractVars_ as
extractVars_ (MyRecord map') = foldMap extractVars_ map'
extractVars_ (MyRecordAccess a _) = extractVars_ a
extractVars_ (MyData _ _ _ a) = extractVars_ a
extractVars_ (MyConstructor _) = mempty
extractVars_ (MyConsApp a b) = extractVars_ a <> extractVars_ b
extractVars_ (MyCaseMatch sum' matches catchAll) =
  extractVars sum'
    <> mconcat (extractVars . snd <$> matches)
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
  let findHash name = do
        hash' <- findHashInBindings bindings' name
        pure (name, hash')
  hashes <- traverse findHash (S.toList . extractVars $ expr)
  Right (StoreExpression (Bindings (M.fromList hashes)) expr)
