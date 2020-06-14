{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Store.Resolver where

import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Language.Mimsa.Syntax
--
--
-- this takes the expression, works out what it needs from it's environment
-- and wraps that up
-- this would be a good place for a simplifying step in future
-- by replacing all variables internally with a1, a2 etc, we'll get less
-- repetition, ie \x -> x and \y -> y will be the same function and thus hash

import Language.Mimsa.Types

-- important - we must not count variables brought in via lambdas, as those
-- aren't external deps

extractVars :: Expr -> Set Name
extractVars (MyVar a) = S.singleton a
extractVars (MyIf a b c) = extractVars a <> extractVars b <> extractVars c
extractVars (MyLet newVar a b) = S.delete newVar (extractVars a <> extractVars b)
extractVars (MyLambda newVar a) = S.delete newVar (extractVars a)
extractVars (MyApp a b) = extractVars a <> extractVars b
extractVars _ = mempty

findHashInBindings :: Bindings -> Name -> Either Text ExprHash
findHashInBindings (Bindings bindings') name = case M.lookup name bindings' of
  Just a -> Right a
  _ -> Left $ "A binding for " <> prettyPrint name <> " could not be found"

-- given an expression, and the current environment, create a
-- store expression that captures the hashes of the functions we'll need
createStoreExpression :: Bindings -> Expr -> Either Text StoreExpression
createStoreExpression bindings' expr = do
  let findHash name =
        (,) <$> pure name
          <*> findHashInBindings bindings' name
  hashes <- traverse findHash (S.toList . extractVars $ expr)
  Right (StoreExpression (M.fromList hashes) expr)
