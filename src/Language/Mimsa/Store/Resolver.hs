{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Store.Resolver where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
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

-- given an expression, and the current environment, create a
-- store expression that captures the hashes of the functions we'll need
createStoreExpression :: StoreEnv -> Expr -> Either Text StoreExpression
createStoreExpression _ (MyInt a) = Right (StoreExpression mempty (MyInt a))
createStoreExpression _ (MyBool a) = Right (StoreExpression mempty (MyBool a))
createStoreExpression _ (MyString a) = Right (StoreExpression mempty (MyString a))
createStoreExpression _ _ = Left "Not implemented yet"
