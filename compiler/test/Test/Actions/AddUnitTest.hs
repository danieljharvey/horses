{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.AddUnitTest
  ( spec,
  )
where

import Data.Either (isLeft)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.AddUnitTest as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

brokenExpr :: Expr Name Annotation
brokenExpr = MyInfix mempty Equals (int 1) (bool True)

projectStoreSize :: Project ann -> Int
projectStoreSize = length . getStore . prjStore

unitTestsSize :: Project ann -> Int
unitTestsSize = M.size . prjUnitTests

testWithIdInExpr :: Expr Name Annotation
testWithIdInExpr =
  MyInfix
    mempty
    Equals
    (MyApp mempty (MyVar mempty "id") (int 1))
    (int 1)

spec :: Spec
spec = do
  describe "AddUnitTest" $ do
    it "Fails with broken test" $ do
      Actions.run
        testStdlib
        (Actions.addUnitTest brokenExpr (TestName "Oh no") "1 == True")
        `shouldSatisfy` isLeft
    it "Adds a new test" $ do
      case Actions.run
        testStdlib
        (Actions.addUnitTest testWithIdInExpr (TestName "Id does nothing") "id(1) == 1") of
        Left _ -> error "Should not have failed"
        Right (newProject, outcomes, _) -> do
          -- one more item in store
          projectStoreSize newProject
            `shouldBe` projectStoreSize testStdlib + 1
          -- one more unit test
          unitTestsSize newProject
            `shouldBe` unitTestsSize testStdlib + 1
          -- new expression
          S.size (Actions.storeExpressionsFromOutcomes outcomes) `shouldBe` 1
