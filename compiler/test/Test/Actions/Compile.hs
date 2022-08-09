{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.Compile
  ( spec,
  )
where

import Data.Either (isRight)
import Data.Foldable
import Data.Functor
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Compile as Actions
import qualified Language.Mimsa.Actions.Evaluate as Actions
import qualified Language.Mimsa.Actions.Helpers.LookupExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Backend.Types
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "Compile" $ do
    it "Simplest compilation creates four files" $ do
      let expr = MyVar mempty Nothing "id"
      let action = do
            (_, _, storeExpr, _, _) <- Actions.evaluate (prettyPrint expr) expr
            Actions.compileStoreExpression Typescript storeExpr
      let (newProject, outcomes, (_, hashes)) =
            fromRight (Actions.run testStdlib action)
      -- creates four files
      length (Actions.writeFilesFromOutcomes outcomes) `shouldBe` 4
      -- optimisations change project
      newProject `shouldNotBe` testStdlib
      -- should have returned two exprHashs (one for the main expr, one
      -- for the `id` dependency
      S.size hashes `shouldBe` 2

    it "Complex compilation creates many files" $ do
      let expr = MyVar mempty Nothing "evalState"
      let action = do
            (_, _, storeExpr, _, _) <- Actions.evaluate (prettyPrint expr) expr
            Actions.compileStoreExpression Typescript storeExpr
      let (newProject, outcomes, _) = fromRight (Actions.run testStdlib action)
      -- creates 9 files
      length (Actions.writeFilesFromOutcomes outcomes) `shouldBe` 9
      -- optimisations change project
      newProject `shouldNotBe` testStdlib

    it "Doesn't break when using bindings that aren't in the store" $ do
      let expr = MyVar mempty Nothing "id2"
      let exprHashForId = getHashOfName testStdlib "id"
      let bindings = M.singleton (Nothing, "id2") exprHashForId
      let storeExpr = StoreExpression expr bindings mempty mempty mempty
      let action = do
            Actions.compileStoreExpression Typescript storeExpr
      Actions.run testStdlib action `shouldSatisfy` isRight

    it "Compiles with deep deps" $ do
      let expr = unsafeParseExpr "either.fmap (\\a -> a + 1) (Right 100)" $> mempty
          action = do
            (_, _, storeExpr, _, _) <- Actions.evaluate (prettyPrint expr) expr
            Actions.compileStoreExpression Typescript storeExpr
      let (_, outcomes, _) = fromRight (Actions.run testStdlib action)
      -- creates 9 files (7 expressions, stdlib, index)
      -- this will reduce once we inline across expressions as
      -- most of this is unneeded `either` functions
      length (Actions.writeFilesFromOutcomes outcomes) `shouldBe` 9

    it "Compiles entire project" $ do
      let action = do
            _ <- Actions.compileProject Typescript
            pure ()
      Actions.run stdlib action `shouldSatisfy` isRight

    describe "Can compile each top-level module" $ do
      let compileModule (modName, modHash) =
            it ("Compiles module " <> T.unpack (prettyPrint modName) <> " from stdlib") $ do
              let action =
                    Actions.lookupModule modHash
                      >>= Actions.compileModule Typescript
              Actions.run stdlib action
                `shouldSatisfy` isRight
      let moduleNames = M.toList . getCurrentModules . prjModules $ stdlib
       in traverse_ compileModule moduleNames
