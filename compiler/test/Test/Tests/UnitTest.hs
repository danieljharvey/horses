{-# LANGUAGE OverloadedStrings #-}

module Test.Tests.UnitTest
  ( spec,
  )
where

import Data.Either (isLeft)
import qualified Data.Map as M
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store
import Language.Mimsa.Tests.Test
import Language.Mimsa.Tests.UnitTest
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
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
  ( mkStoreExpression
      testExpr
  )
    { storeBindings =
        M.singleton (Nothing, "incrementInt") incrementIntH
    }

idHash :: ExprHash
idHash = getHashOfName testStdlib "id"

testingIdExpr :: Expr Name Annotation
testingIdExpr =
  MyInfix
    mempty
    Equals
    (MyApp mempty (MyVar mempty Nothing "id") (int 100))
    (int 100)

testingStoreExpr :: StoreExpression Annotation
testingStoreExpr =
  ( mkStoreExpression
      testingIdExpr
  )
    { storeBindings =
        M.singleton (Nothing, "id") idHash
    }

altIdStoreExpr :: StoreExpression Annotation
altIdStoreExpr =
  mkStoreExpression
    ( MyLambda
        mempty
        (Identifier mempty "b")
        (MyVar mempty Nothing "b")
    )

altIdHash :: ExprHash
altIdHash = getStoreExpressionHash altIdStoreExpr

projectStoreSize :: Project ann -> Int
projectStoreSize = length . getStore . prjStore

testCount :: Project ann -> Int
testCount = length . prjTests

spec :: Spec
spec =
  describe "UnitTest" $ do
    describe "createNewTests" $ do
      it "Returns empty when no previous unit tests match" $ do
        createNewTests testStdlib idHash idHash `shouldBe` Right (testStdlib, mempty)

      it "Replacing a test with itself is a no-op" $ do
        let firstTest =
              createTestOrExplode
                testStdlib
                testingStoreExpr
                (TestName "id does nothing")
        let testStdlibWithTest = testStdlib <> fromTest (UTest firstTest) testingStoreExpr
        createNewTests testStdlibWithTest idHash idHash
          `shouldBe` Right (testStdlibWithTest, [testingStoreExpr])

      it "Updating a test adds new unit tests and items to the Store" $ do
        let firstTest =
              createTestOrExplode
                testStdlib
                testingStoreExpr
                (TestName "id does nothing")
        let testStdlibWithTest =
              testStdlib
                <> fromTest (UTest firstTest) testingStoreExpr
                <> fromItem "id" altIdStoreExpr altIdHash
        let (prj, exprs) = case createNewTests testStdlibWithTest idHash altIdHash of
              Right (a, b) -> (a, b)
              Left _ -> (undefined, undefined)
        -- we've added one item to the store
        projectStoreSize prj `shouldSatisfy` \a -> a == projectStoreSize testStdlibWithTest + 1
        -- we've added another unit tests
        testCount prj `shouldSatisfy` \i -> i == testCount testStdlibWithTest + 1
        -- there is one store expression returned
        exprs `shouldSatisfy` \a -> length a == 1
        -- and it's different to the original one
        exprs `shouldSatisfy` \a -> a /= [testingStoreExpr]

    describe "getTestsForExprHash" $ do
      it "Returns none when there are no tests" $ do
        getTestsForExprHash testStdlib (ExprHash "123") `shouldBe` mempty

      it "Returns incrementInt test when passed its hash" $ do
        let unitTest =
              createTestOrExplode
                testStdlib
                testStoreExpr
                (TestName "incrementInt is a no-op")
        let testStdlib' = fromTest (UTest unitTest) testStoreExpr <> testStdlib
        getTestsForExprHash testStdlib' incrementIntH
          `shouldBe` M.singleton (utExprHash unitTest) (UTest unitTest)

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
