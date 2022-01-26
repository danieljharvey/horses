{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.Optimise
  ( spec,
  )
where

import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Optimise as Actions
import Language.Mimsa.Project.Versions
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

useIdPointlessly :: StoreExpression Annotation
useIdPointlessly =
  let expr = unsafeParseExpr "let useless = id 100 in True" $> mempty
   in StoreExpression expr (Bindings $ M.singleton "id" (ExprHash "123")) mempty

trueExpr :: Expr Name Annotation
trueExpr = unsafeParseExpr "True" $> mempty

wontOptimise :: StoreExpression Annotation
wontOptimise =
  let expr = unsafeParseExpr "let useless = id 100 in useless" $> mempty
   in StoreExpression expr (Bindings $ M.singleton "id" (ExprHash "123")) mempty

withLambda :: StoreExpression Annotation
withLambda =
  let expr = unsafeParseExpr "\\a -> let useless = True in 200" $> mempty
   in StoreExpression expr mempty mempty

optimisedLambda :: Expr Name Annotation
optimisedLambda =
  unsafeParseExpr "\\a -> 200" $> mempty

spec :: Spec
spec = do
  describe "Optimise" $ do
    it "Successfully optimising doesn't change" $ do
      let action = do
            Actions.appendStoreExpression wontOptimise
            Actions.optimise wontOptimise
      let (prj, _actions, StoreExpression newExpr (Bindings bindings) _) =
            fromRight $ Actions.run testStdlib action
      -- updated expr
      newExpr `shouldBe` storeExpression wontOptimise
      -- new store expression has no deps
      M.size bindings `shouldBe` 1
      -- only the manually added store expression added
      additionalStoreItems testStdlib prj `shouldBe` 1

    it "Successfully optimises away unused variable and dep" $ do
      let action = do
            Actions.optimise useIdPointlessly
      let (prj, _actions, StoreExpression newExpr (Bindings bindings) _) =
            fromRight $ Actions.run testStdlib action
      -- updated expr
      newExpr `shouldBe` trueExpr
      -- new store expression has no deps
      M.null bindings `shouldBe` True
      -- stored new expression
      additionalStoreItems testStdlib prj `shouldBe` 1

    it "Optimise by name" $ do
      let action = do
            Actions.bindStoreExpression withLambda "useId"
            Actions.optimiseByName "useId"
      let (prj, _actions, StoreExpression newExpr (Bindings bindings) _) =
            fromRight $ Actions.run testStdlib action
      -- updated expr
      newExpr `shouldBe` optimisedLambda
      -- new store expression has no deps
      M.null bindings `shouldBe` True
      -- stored new expression
      additionalStoreItems testStdlib prj `shouldBe` 2
      -- there are two versions of binding
      let boundExprHashes = fromRight $ findInProject prj "useId"
      NE.length boundExprHashes `shouldBe` 2
