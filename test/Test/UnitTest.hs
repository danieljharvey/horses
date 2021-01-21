{-# LANGUAGE OverloadedStrings #-}

module Test.UnitTest
  ( spec,
  )
where

import Data.Either (isLeft)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Project.UnitTest
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

getHashOfName :: Project ann -> Name -> ExprHash
getHashOfName prj name =
  case lookupBindingName prj name of
    Just a -> a
    _ -> error "could not getHashOfName"

createTestOrExplode ::
  Project Annotation ->
  StoreExpression Annotation ->
  TestName ->
  UnitTest
createTestOrExplode prj sExpr name = case createUnitTest prj sExpr name of
  Right a -> a
  _ -> error "EXPLODE"

testExpr :: Expr Name Annotation
testExpr =
  MyInfix
    mempty
    Equals
    (int 1)
    (MyApp mempty (MyVar mempty (mkName "incrementInt")) (int 1))

incrementIntH :: ExprHash
incrementIntH = getHashOfName stdLib (mkName "incrementInt")

testStoreExpr :: StoreExpression Annotation
testStoreExpr =
  StoreExpression
    testExpr
    (Bindings $ M.singleton (mkName "incrementInt") incrementIntH)
    mempty

idHash :: ExprHash
idHash = getHashOfName stdLib (mkName "id")

testingIdExpr :: Expr Name Annotation
testingIdExpr =
  MyInfix
    mempty
    Equals
    (MyApp mempty (MyVar mempty (mkName "id")) (int 100))
    (int 100)

testingStoreExpr :: StoreExpression Annotation
testingStoreExpr =
  StoreExpression
    testingIdExpr
    (Bindings $ M.singleton (mkName "id") idHash)
    mempty

altIdStoreExpr :: StoreExpression Annotation
altIdStoreExpr =
  StoreExpression
    ( MyLambda
        mempty
        (mkName "b")
        (MyVar mempty (mkName "b"))
    )
    mempty
    mempty

altIdHash :: ExprHash
altIdHash = getStoreExpressionHash altIdStoreExpr

projectStoreSize :: Project ann -> Int
projectStoreSize = length . getStore . store

unitTestCount :: Project ann -> Int
unitTestCount = length . prjUnitTests

spec :: Spec
spec =
  describe "UnitTest" $ do
    describe "createNewUnitTests" $ do
      it "Returns empty when no previous unit tests match" $ do
        createNewUnitTests stdLib idHash idHash `shouldBe` Right (stdLib, mempty)
      it "Replacing a test with itself is a no-op" $ do
        let firstTest =
              createTestOrExplode
                stdLib
                testingStoreExpr
                (TestName "id does nothing")
        let stdLibWithTest = stdLib <> fromUnitTest firstTest testingStoreExpr
        createNewUnitTests stdLibWithTest idHash idHash
          `shouldBe` Right (stdLibWithTest, [testingStoreExpr])
      it "Updating a test adds new unit tests and items to the Store" $ do
        let firstTest =
              createTestOrExplode
                stdLib
                testingStoreExpr
                (TestName "id does nothing")
        let stdLibWithTest =
              stdLib
                <> fromUnitTest firstTest testingStoreExpr
                <> fromItem (mkName "id") altIdStoreExpr altIdHash
        let (prj, exprs) = case createNewUnitTests stdLibWithTest idHash altIdHash of
              Right (a, b) -> (a, b)
              Left _ -> (undefined, undefined)
        -- we've added one item to the store
        projectStoreSize prj `shouldSatisfy` \a -> a == projectStoreSize stdLibWithTest + 1
        -- we've added another unit tests
        unitTestCount prj `shouldSatisfy` \i -> i == unitTestCount stdLibWithTest + 1
        -- there is one store expression returned
        exprs `shouldSatisfy` \a -> length a == 1
        -- and it's different to the original one
        exprs `shouldSatisfy` \a -> a /= [testingStoreExpr]
    describe "getTestsForExprHash" $ do
      it "Returns none when there are no tests" $ do
        getTestsForExprHash stdLib (ExprHash "123") `shouldBe` mempty
      it "Returns incrementInt test when passed its hash" $ do
        let unitTest =
              createTestOrExplode
                stdLib
                testStoreExpr
                (TestName "incrementInt is a no-op")
        let stdLib' = fromUnitTest unitTest testStoreExpr <> stdLib
        getTestsForExprHash stdLib' incrementIntH
          `shouldBe` M.singleton (utExprHash unitTest) unitTest
    describe "createUnitTest" $ do
      it "True is a valid test" $ do
        let storeExpr = StoreExpression (bool True) mempty mempty
        createUnitTest stdLib storeExpr (TestName "True is true")
          `shouldBe` Right
            ( UnitTest
                (TestName "True is true")
                (TestSuccess True)
                (getStoreExpressionHash storeExpr)
                mempty
            )
      it "False is a valid (but failing) test" $ do
        let storeExpr = StoreExpression (bool False) mempty mempty
        createUnitTest stdLib storeExpr (TestName "False is not true")
          `shouldBe` Right
            ( UnitTest
                (TestName "False is not true")
                (TestSuccess False)
                (getStoreExpressionHash storeExpr)
                mempty
            )
      it "100 is not a valid test" $ do
        let storeExpr = StoreExpression (int 100) mempty mempty
        createUnitTest stdLib storeExpr (TestName "100 is not a valid test")
          `shouldSatisfy` isLeft

      it "Finds incrementInt and addInt" $ do
        createUnitTest stdLib testStoreExpr (TestName "incrementInt is a no-op")
          `shouldBe` Right
            ( UnitTest
                (TestName "incrementInt is a no-op")
                (TestSuccess False)
                (getStoreExpressionHash testStoreExpr)
                ( S.fromList
                    [ incrementIntH
                    ]
                )
            )
