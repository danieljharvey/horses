{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.AddUnitTest
  ( spec,
  )
where

import Data.Either (isLeft)
import Data.Functor
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.AddUnitTest as Actions
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.UnitTest
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

brokenExpr :: Expr Name Annotation
brokenExpr = MyInfix mempty Equals (int 1) (bool True)

additionalUnitTests :: Project ann -> Project ann -> Int
additionalUnitTests old new =
  unitTestsSize new - unitTestsSize old
  where
    unitTestsSize :: Project ann -> Int
    unitTestsSize = M.size . prjUnitTests

additionalStoreItems :: Project ann -> Project ann -> Int
additionalStoreItems old new =
  projectStoreSize new - projectStoreSize old
  where
    projectStoreSize :: Project ann -> Int
    projectStoreSize = length . getStore . prjStore

testWithIdInExpr :: Expr Name Annotation
testWithIdInExpr =
  MyInfix
    mempty
    Equals
    (MyApp mempty (MyVar mempty "id") (int 1))
    (int 1)

testWithIdAndConst :: Expr Name Annotation
testWithIdAndConst = unsafeParseExpr "id 1 == (const 1 False)" $> mempty

idHash :: ExprHash
idHash = getHashOfName testStdlib "id"

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
        (Actions.addUnitTest testWithIdInExpr (TestName "Id does nothing") "id 1 == 1") of
        Left _ -> error "Should not have failed"
        Right (newProject, outcomes, _) -> do
          -- one more item in store
          additionalStoreItems testStdlib newProject
            `shouldBe` 1
          -- one more unit test
          additionalUnitTests testStdlib newProject
            `shouldBe` 1
          -- new expression
          S.size (Actions.storeExpressionsFromOutcomes outcomes) `shouldBe` 1
    it "Adds a new test, updates it's dep, but retrieving only returns one version" $ do
      let newConst = MyLambda mempty "aaa" (MyLambda mempty "bbb" (MyVar mempty "aaa"))
      case Actions.run
        testStdlib
        ( do
            _ <- Actions.addUnitTest testWithIdAndConst (TestName "Id does nothing") (prettyPrint testWithIdAndConst)
            Actions.bindExpression newConst "const" (prettyPrint newConst)
        ) of
        Left e -> error (T.unpack $ prettyPrint e)
        Right (newProject, _, _) -> do
          additionalUnitTests testStdlib newProject `shouldBe` 2
          -- When actually fetching tests we should only show one for id
          -- instead of for both versions of `const`
          let gotTests = getTestsForExprHash newProject idHash
          length gotTests `shouldBe` 1
