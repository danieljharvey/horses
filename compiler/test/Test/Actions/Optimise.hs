{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.Optimise
  ( spec,
  )
where

import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Optimise as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Versions
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

idHash :: ExprHash
idHash = getHashOfName testStdlib "id"

useIdPointlessly :: StoreExpression Annotation
useIdPointlessly =
  let expr = unsafeParseExpr "let useless = id 100 in True" $> mempty
   in StoreExpression expr (Bindings $ M.singleton "id" idHash) mempty

trueExpr :: Expr Name Annotation
trueExpr = unsafeParseExpr "True" $> mempty

withLambda :: StoreExpression Annotation
withLambda =
  let expr = unsafeParseExpr "\\a -> let useless = True in 200" $> mempty
   in StoreExpression expr mempty mempty

optimisedLambda :: Expr Name Annotation
optimisedLambda =
  unsafeParseExpr "\\a -> 200" $> mempty

useTrueVal :: Expr Name Annotation
useTrueVal =
  unsafeParseExpr "if trueVal then 1 else 2" $> mempty

spec :: Spec
spec = do
  fdescribe "Optimise" $ do
    it "Successfully optimises away unused variable and dep" $ do
      let action = do
            Actions.optimise useIdPointlessly
      let (prj, _actions, (resolved, _)) =
            fromRight $ Actions.run testStdlib action
      let (StoreExpression newExpr (Bindings bindings) _) = reStoreExpression resolved

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
      let (prj, _actions, (resolved, _)) =
            fromRight $ Actions.run testStdlib action
      let (StoreExpression newExpr (Bindings bindings) _) = reStoreExpression resolved

      -- updated expr
      newExpr `shouldBe` optimisedLambda
      -- new store expression has no deps
      M.null bindings `shouldBe` True
      -- stored new expression
      additionalStoreItems testStdlib prj `shouldBe` 2
      -- there are two versions of binding
      let boundExprHashes = fromRight $ findInProject prj "useId"
      NE.length boundExprHashes `shouldBe` 2
      -- current hash is new one
      let newBoundHash = getHashOfName prj "useId"
      newBoundHash `shouldBe` getStoreExpressionHash (reStoreExpression resolved)
      -- one new optimisation
      additionalOptimisations testStdlib prj `shouldBe` 1

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

    it "Optimising inlines a small dep" $ do
      let action = do
            Actions.bindStoreExpression (StoreExpression trueExpr mempty mempty) "trueVal"
            _ <- Actions.bindExpression useTrueVal "useTrueVal" (prettyPrint useTrueVal)
            Actions.optimiseByName "useTrueVal"
      let (prj, _actions, _) =
            fromRight $ Actions.run testStdlib action

      -- lookup 'useTrueVal' in prj
      let useTrueValHash = getHashOfName prj "useTrueVal"
      -- check it has no deps because they got inlined
      case M.lookup useTrueValHash (getStore $ prjStore prj) of
        Just se -> do
          M.size (getBindings $ storeBindings se) `shouldBe` 0
        _ -> error "Did not find useTrueVal store expression"

    it "Optimising inlines a big dep and brings in it's deps" $ do
      let action = do
            let useEitherFmap = unsafeParseExpr "either.fmap (\\a -> a + 1) (Right 1)" $> mempty
            _ <- Actions.bindExpression useEitherFmap "useEitherFmap" (prettyPrint useEitherFmap)
            Actions.optimiseByName "useEitherFmap"

      let (prj, _actions, _) =
            fromRight $ Actions.run testStdlib action

      -- lookup 'useEitherFmap' in prj
      let useTrueValHash = getHashOfName prj "useEitherFmap"
      -- check it has no deps because they got inlined
      case M.lookup useTrueValHash (getStore $ prjStore prj) of
        Just se -> do
          M.size (getBindings $ storeBindings se) `shouldBe` 0
        _ -> error "Did not find useEitherFmat store expression"
