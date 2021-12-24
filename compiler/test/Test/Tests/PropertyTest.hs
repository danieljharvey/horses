{-# LANGUAGE OverloadedStrings #-}

module Test.Tests.PropertyTest
  ( spec,
  )
where

import Control.Monad.Except
import Data.Either (isLeft, isRight)
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Tests.PropertyTest
import Language.Mimsa.Tests.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

constTrueHash :: ExprHash
constTrueHash = getHashOfName testStdlib "constTrue"

constFalseHash :: ExprHash
constFalseHash = getHashOfName testStdlib "constFalse"

invertTreeTwiceHash :: ExprHash
invertTreeTwiceHash = getHashOfName testStdlib "invertTreeTwice"

getStoreItem :: Project ann -> ExprHash -> StoreExpression ann
getStoreItem prj hash =
  case lookupExprHash prj hash of
    Just a -> a
    _ -> error "Error in getStoreItem in PropertyTest tests"

createPropertyTest' :: StoreExpression Annotation -> TestName -> PropertyTest
createPropertyTest' se testName = case createPropertyTest testStdlib se testName of
  Right a -> a
  Left e -> error (T.unpack (prettyPrint e))

-- | run an ExceptT thing and explode on error
runExceptYOLO :: ExceptT (Error Annotation) IO a -> IO a
runExceptYOLO action = do
  res <- runExceptT action
  case res of
    Right a -> pure a
    Left e -> error (T.unpack (prettyPrint e))

spec :: Spec
spec =
  describe "PropertyTest" $ do
    describe "runPropertyTest" $ do
      it "Runs a passing test" $ do
        let pt =
              createPropertyTest'
                (getStoreItem testStdlib constTrueHash)
                (TestName "const true")
        result <-
          runExceptYOLO $
            runPropertyTest testStdlib pt
        result `shouldBe` PropertyTestSuccess

      it "Runs a passing test that uses a datatype" $ do
        let pt =
              createPropertyTest'
                (getStoreItem testStdlib invertTreeTwiceHash)
                (TestName "inverting a tree twice is identity")
        result <-
          runExceptYOLO $
            runPropertyTest testStdlib pt
        result `shouldBe` PropertyTestSuccess

      it "Runs a failing test" $ do
        let pt =
              createPropertyTest'
                (getStoreItem testStdlib constFalseHash)
                (TestName "const false")
        result <-
          runExceptYOLO $
            runPropertyTest testStdlib pt
        result `shouldBe` PropertyTestFailures (S.fromList [bool True, bool False])

    describe "createPropertyTest" $ do
      it "True is not a valid property test" $ do
        let expr = bool True
            storeExpr = StoreExpression expr mempty mempty
        createPropertyTest testStdlib storeExpr (TestName "It's a mess")
          `shouldSatisfy` isLeft

      it "\\bool -> True is a valid property test" $ do
        let expr = MyLambda mempty (Identifier mempty "bool") (bool True)
            storeExpr = StoreExpression expr mempty mempty
        createPropertyTest testStdlib storeExpr (TestName "It's always true")
          `shouldSatisfy` isRight

      it "\\bool -> False is a valid property test" $ do
        let expr = MyLambda mempty (Identifier mempty "bool") (bool False)
            storeExpr = StoreExpression expr mempty mempty
        createPropertyTest testStdlib storeExpr (TestName "It's always false")
          `shouldSatisfy` isRight
