{-# LANGUAGE OverloadedStrings #-}

module Test.Tests.UnitTest
  ( spec,
  )
where

import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import Language.Mimsa.Store
import Language.Mimsa.Tests.UnitTest
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Tests
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

testExpr :: Expr Name Annotation
testExpr =
  MyInfix
    mempty
    Equals
    (int 1)
    (MyApp mempty (MyVar mempty Nothing "incrementInt") (int 1))

incrementIntH :: ExprHash
incrementIntH = getHashOfName testStdlib "incrementInt"

testStoreExpr :: StoreExpression Annotation
testStoreExpr =
  StoreExpression
    { storeExpression = testExpr,
      storeTypeBindings = mempty,
      storeTypes = mempty,
      storeInfixes = mempty,
      storeBindings =
        M.singleton (Nothing, "incrementInt") incrementIntH
    }

spec :: Spec
spec =
  describe "UnitTest" $ do
    describe "createUnitTest" $ do
      it "True is a valid test" $ do
        let storeExpr = mkStoreExpression (bool True)
        createUnitTest testStdlib storeExpr (TestName "True is true")
          `shouldBe` Right
            ( UnitTest
                (TestName "True is true")
                (UnitTestSuccess True)
                (getStoreExpressionHash storeExpr)
            )
      it "False is a valid (but failing) test" $ do
        let storeExpr = mkStoreExpression (bool False)
        createUnitTest testStdlib storeExpr (TestName "False is not true")
          `shouldBe` Right
            ( UnitTest
                (TestName "False is not true")
                (UnitTestSuccess False)
                (getStoreExpressionHash storeExpr)
            )
      it "100 is not a valid test" $ do
        let storeExpr = mkStoreExpression (int 100)
        createUnitTest testStdlib storeExpr (TestName "100 is not a valid test")
          `shouldSatisfy` isLeft

      it "\\bool -> True is not a valid unit test" $ do
        let expr = MyLambda mempty (Identifier mempty "bool") (bool True)
            storeExpr = mkStoreExpression expr
        createUnitTest testStdlib storeExpr (TestName "It's always true")
          `shouldSatisfy` isLeft

      it "Finds incrementInt and addInt" $ do
        createUnitTest testStdlib testStoreExpr (TestName "incrementInt is a no-op")
          `shouldBe` Right
            ( UnitTest
                (TestName "incrementInt is a no-op")
                (UnitTestSuccess False)
                (getStoreExpressionHash testStoreExpr)
            )
