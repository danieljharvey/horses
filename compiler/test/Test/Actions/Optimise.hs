{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.Optimise
  ( spec,
  )
where

import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Optimise as Actions
import Language.Mimsa.Project.Versions
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers
import Language.Mimsa.Project.Stdlib

idHash :: ExprHash
idHash = getHashOfName testStdlib "id"

useIdPointlessly :: StoreExpression Annotation
useIdPointlessly =
  let expr = unsafeParseExpr "let useless = id 100 in True" $> mempty
   in StoreExpression
        expr
        (M.singleton (Nothing, "id") idHash)
        mempty
        mempty
        mempty

trueExpr :: Expr Name Annotation
trueExpr = unsafeParseExpr "True" $> mempty

withLambda :: StoreExpression Annotation
withLambda =
  let expr = unsafeParseExpr "\\a -> let useless = True in 200" $> mempty
   in mkStoreExpression expr

optimisedLambda :: Expr Name Annotation
optimisedLambda =
  unsafeParseExpr "\\a -> 200" $> mempty

spec :: Spec
spec = do
  describe "Optimise" $ do
    it "Successfully optimises away unused variable and dep" $ do
      let action = do
            Actions.optimise useIdPointlessly
      let (prj, _actions, resolved) =
            fromRight $ Actions.run stdlib action
      let newSe = reStoreExpression resolved

      -- updated expr
      storeExpression newSe `shouldBe` Just trueExpr
      -- new store expression has no deps
      M.null (storeBindings newSe) `shouldBe` True
      -- stored new expression
      additionalStoreItems testStdlib prj `shouldBe` 1

    it "Optimise by name" $ do
      let action = do
            Actions.bindStoreExpression withLambda "useId"
            Actions.optimiseByName "useId"
      let (prj, _actions, resolved) =
            fromRight $ Actions.run testStdlib action
      let newSe = reStoreExpression resolved

      -- updated expr
      storeExpression newSe `shouldBe` Just optimisedLambda
      -- new store expression has no deps
      M.null (storeBindings newSe) `shouldBe` True
      -- stored new expression
      additionalStoreItems testStdlib prj `shouldBe` 2
      -- there are two versions of binding
      let boundExprHashes = fromRight $ findInProject prj "useId"
      NE.length boundExprHashes `shouldBe` 2
      -- current hash is new one
      let newBoundHash = getHashOfName prj "useId"
      newBoundHash `shouldBe` getStoreExpressionHash (reStoreExpression resolved)

    it "Optimising twice returns same store expression and does not repeat work" $ do
      let action = do
            Actions.bindStoreExpression withLambda "useId"
            Actions.optimiseByName "useId"
      let (prj, _actions, _) =
            fromRight $ Actions.run testStdlib action

      let action2 = do
            Actions.optimiseByName "useId"

      let (prj2, _actions, _) =
            fromRight $ Actions.run prj action2

      -- no new expressions on second run
      additionalStoreItems prj prj2 `shouldBe` 0
      -- current hash has not changed
      getHashOfName prj "useId" `shouldBe` getHashOfName prj2 "useId"
