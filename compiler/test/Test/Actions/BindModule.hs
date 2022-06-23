{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.BindModule
  ( spec,
  )
where

import Data.Either (isLeft)
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.BindModule as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Project
import Test.Data.Prelude
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

newModules :: Project ann -> Project ann -> Int
newModules old new =
  let countModules = length . prjModuleStore
   in countModules new - countModules old

spec :: Spec
spec = do
  describe "BindModule" $ do
    describe "addBindingToModule" $ do
      it "Adds a new function to Prelude that does not typecheck" $ do
        let action = do
              (_hash, mod') <- Actions.bindModule prelude "Prelude" (prettyPrint prelude)
              let input = "id True False"
                  modItem = ModuleExpression "idTrue" [] (unsafeParseExpr' input)
              Actions.addBindingToModule mod' modItem input
        Actions.run testStdlib action `shouldSatisfy` isLeft

      it "Adds a new function to Prelude that is good typecheck" $ do
        let action = do
              (_hash, mod') <- Actions.bindModule prelude "Prelude" (prettyPrint prelude)
              let input = "id True False"
                  expr = unsafeParseExpr' input
              Actions.addBindingToModule mod' "useId" expr input
        let (newProject, _outcomes, _) = fromRight $ Actions.run testStdlib action
        newModules testStdlib newProject
          `shouldBe` 1

    describe "bindModule" $ do
      it "Adds a fresh new module to prjModules and to Store" $ do
        case Actions.run testStdlib (Actions.bindModule prelude "Prelude" (prettyPrint prelude)) of
          Left _ -> expectationFailure "Should not have failed"
          Right (newProject, outcomes, _) -> do
            -- one more item in module store
            newModules testStdlib newProject
              `shouldBe` 1
            -- one more binding
            lookupModuleName
              newProject
              "Prelude"
              `shouldSatisfy` isRight
            -- one new store expression
            S.size (Actions.modulesFromOutcomes outcomes)
              `shouldBe` 1
