{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.Evaluate
  ( spec,
  )
where

import Data.Either (isLeft, isRight)
import Data.Functor
import qualified Language.Mimsa.Actions.BindModule as Actions
import qualified Language.Mimsa.Actions.Evaluate as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

brokenExpr :: Expr Name Annotation
brokenExpr = MyInfix mempty Equals (int 1) (bool True)

onePlusOneExpr :: Expr Name Annotation
onePlusOneExpr = MyInfix mempty Add (int 1) (int 1)

spec :: Spec
spec = do
  describe "Evaluate" $ do
    describe "StoreExpression-based" $ do
      it "Should return an error for a broken expr" $ do
        Actions.run testStdlib (Actions.evaluate (prettyPrint brokenExpr) brokenExpr) `shouldSatisfy` isLeft

      it "Should evaluate an expression" $ do
        let action = Actions.evaluate (prettyPrint onePlusOneExpr) onePlusOneExpr
        let (newProject, _, (mt, expr, _, _, _)) = fromRight (Actions.run testStdlib action)
        mt $> () `shouldBe` MTPrim mempty MTInt
        expr $> () `shouldBe` int 2
        -- optimised version in store
        additionalStoreItems testStdlib newProject `shouldBe` 1

      it "Should evaluate an expression with a nested dependency" $ do
        let expr = unsafeParseExpr "incrementInt 1 + id 10" $> mempty
        let action = Actions.evaluate (prettyPrint expr) expr
        let (_, _, (mt, newExpr, _, _, _)) = fromRight (Actions.run testStdlib action)
        mt $> () `shouldBe` MTPrim mempty MTInt
        newExpr $> () `shouldBe` int 12

    describe "Module-based" $ do
      it "Should use the passed in module as context" $ do
        let action = do
              -- add a definition to an empty module
              let expr = unsafeParseModuleItem "def dog = True"
              newMod <- Actions.addBindingToModule mempty expr ""
              -- evaluate using that module
              Actions.evaluateModule "dog" (unsafeParseExpr' "dog") (getAnnotationForType <$> newMod)
        Actions.run testStdlib action `shouldSatisfy` isRight
