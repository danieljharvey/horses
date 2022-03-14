{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.Compile
  ( spec,
  )
where

import Data.Either (isRight)
import Data.Functor
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Compile as Actions
import qualified Language.Mimsa.Actions.Evaluate as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Backend.Types
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "Compile" $ do
    it "Simplest compilation creates four files" $ do
      let expr = MyVar mempty "id"
      let action = do
            (_, _, storeExpr, _, _) <- Actions.evaluate (prettyPrint expr) expr
            Actions.compile Typescript storeExpr
      let (newProject, outcomes, (_, hashes)) =
            fromRight (Actions.run testStdlib action)
      -- creates four files
      length (Actions.writeFilesFromOutcomes outcomes) `shouldBe` 4
      -- optimisations change project
      newProject `shouldNotBe` testStdlib
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

    it "Complex compilation creates many files in 2 folders" $ do
      let expr = unsafeParseExpr "state.eval" $> mempty
      let action = do
            (_, _, storeExpr, _, _) <- Actions.evaluate (prettyPrint expr) expr
            Actions.compile Typescript storeExpr
      let (newProject, outcomes, _) = fromRight (Actions.run testStdlib action)
      -- creates 9 files
      length (Actions.writeFilesFromOutcomes outcomes) `shouldBe` 9
      -- optimisations change project
      newProject `shouldNotBe` testStdlib
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
            Actions.compile Typescript storeExpr
      Actions.run testStdlib action `shouldSatisfy` isRight

    it "Compiles with deep deps" $ do
      let expr = unsafeParseExpr "either.fmap (\\a -> a + 1) (Right 100)" $> mempty
          action = do
            (_, _, storeExpr, _, _) <- Actions.evaluate (prettyPrint expr) expr
            Actions.compile Typescript storeExpr
      let (_, outcomes, _) = fromRight (Actions.run testStdlib action)
      -- creates 9 files (7 expressions, stdlib, index)
      -- this will reduce once we inline across expressions as
      -- most of this is unneeded `either` functions
      length (Actions.writeFilesFromOutcomes outcomes) `shouldBe` 9
