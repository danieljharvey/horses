{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.Compile
  ( spec,
  )
where

import Data.Either (isLeft)
import Data.List (nub)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Compile as Actions
import qualified Language.Mimsa.Actions.Evaluate as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Test.Data.Project
import Test.Hspec

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
            (_, _, storeExpr) <- Actions.evaluate (prettyPrint expr) expr
            Actions.compile consoleRuntime "1" storeExpr
      let result = Actions.run stdLib action
      result `shouldSatisfy` isLeft
    it "Simplest compilation creates four files" $ do
      let expr = MyVar mempty "id"
      let action = do
            (_, _, storeExpr) <- Actions.evaluate (prettyPrint expr) expr
            Actions.compile exportRuntime "id" storeExpr
      let (newProject, outcomes, (_, hashes)) =
            fromRight (Actions.run stdLib action)
      -- creates three files
      length (Actions.writeFilesFromOutcomes outcomes) `shouldBe` 4
      -- doesn't change project (for now)
      newProject `shouldBe` stdLib
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
            (_, _, storeExpr) <- Actions.evaluate (prettyPrint expr) expr
            Actions.compile exportRuntime "evalState" storeExpr
      let (newProject, outcomes, _) = fromRight (Actions.run stdLib action)
      -- creates six files
      length (Actions.writeFilesFromOutcomes outcomes) `shouldBe` 7
      -- doesn't change project (for now)
      newProject `shouldBe` stdLib
      -- uses three different folders
      let uniqueFolders =
            nub
              ( (\(path, _, _) -> path)
                  <$> Actions.writeFilesFromOutcomes outcomes
              )
      length uniqueFolders `shouldBe` 3
