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
import Language.Mimsa.Tests.PropertyTest
import Language.Mimsa.Tests.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

constTrueHash :: ExprHash
constTrueHash = getHashOfName testStdlib "constTrue"

constFalseHash :: ExprHash
constFalseHash = getHashOfName testStdlib "constFalse"

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
        result <-
          runExceptYOLO $
            runPropertyTest testStdlib (PropertyTest (TestName "const true") constTrueHash mempty)
        result `shouldBe` PropertyTestSuccess

      it "Runs a failing test" $ do
        result <-
          runExceptYOLO $
            runPropertyTest testStdlib (PropertyTest (TestName "const false") constFalseHash mempty)
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
