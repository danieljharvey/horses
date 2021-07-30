{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.Compile
  ( spec,
  )
where

import Data.Either (isLeft, isRight)
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Compile as Actions
import qualified Language.Mimsa.Actions.Evaluate as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

fromRight :: (Printer e) => Either e a -> a
fromRight either' = case either' of
  Left e -> error (T.unpack $ prettyPrint e)
  Right a -> a

spec :: Spec
spec = do
  describe "Compile" $ do
    it "Does not compile when expression does not match runtime" $ do
      let expr :: Expr Name Annotation
          expr = MyLiteral mempty (MyInt 1)
      let action = do
            (_, _, storeExpr, _, _) <- Actions.evaluate (prettyPrint expr) expr
            Actions.compile consoleRuntime "1" storeExpr
      let result = Actions.run testStdlib action
      result `shouldSatisfy` isLeft
    it "Simplest compilation creates four files" $ do
      let expr = MyVar mempty "id"
      let action = do
            (_, _, storeExpr, _, _) <- Actions.evaluate (prettyPrint expr) expr
            Actions.compile exportRuntime "id" storeExpr
      let (newProject, outcomes, (_, hashes)) =
            fromRight (Actions.run testStdlib action)
      -- creates three files
      length (Actions.writeFilesFromOutcomes outcomes) `shouldBe` 4
      -- doesn't change project (for now)
      newProject `shouldBe` testStdlib
      -- uses three different folders
      let uniqueFolders =
            nub
              ( (\(path, _, _) -> path)
                  <$> Actions.writeFilesFromOutcomes outcomes
              )
      length uniqueFolders `shouldBe` 3
      -- should have returned two exprHashs (one for the main expr, one
      -- for the `id` dependency
      S.size hashes `shouldBe` 2
    it "Complex compilation creates many files in 3 folders" $ do
      let expr = MyVar mempty "evalState"
      let action = do
            (_, _, storeExpr, _, _) <- Actions.evaluate (prettyPrint expr) expr
            Actions.compile exportRuntime "evalState" storeExpr
      let (newProject, outcomes, _) = fromRight (Actions.run testStdlib action)
      -- creates six files
      length (Actions.writeFilesFromOutcomes outcomes) `shouldBe` 7
      -- doesn't change project (for now)
      newProject `shouldBe` testStdlib
      -- uses three different folders
      let uniqueFolders =
            nub
              ( (\(path, _, _) -> path)
                  <$> Actions.writeFilesFromOutcomes outcomes
              )
      length uniqueFolders `shouldBe` 3
    it "Doesn't break when using bindings that aren't in the store" $ do
      let expr = MyVar mempty "id2"
      let exprHashForId = getHashOfName testStdlib "id"
      let bindings = Bindings (M.singleton "id2" exprHashForId)
      let storeExpr = StoreExpression expr bindings mempty
      let action = do
            Actions.compile exportRuntime "id2" storeExpr
      Actions.run testStdlib action `shouldSatisfy` isRight
