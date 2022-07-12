{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.BindModule
  ( spec,
  )
where

import Data.Either (isLeft, isRight)
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.BindModule as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Typechecker
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
              let input = "def idTrue = fst True"
                  modItem = unsafeParseModuleItem input
              Actions.addBindingToModule mempty mod' modItem
        Actions.run testStdlib action `shouldSatisfy` isLeft

      it "Adds a new function to Prelude that is good typecheck" $ do
        let action = do
              (_hash, mod') <- Actions.bindModule prelude "Prelude" (prettyPrint prelude)
              let input = "def useId = fst (True, 1)"
                  modItem = unsafeParseModuleItem input
              (newModule, _) <- Actions.addBindingToModule mempty mod' modItem
              Actions.bindModule (getAnnotationForType <$> newModule) "Repl" (prettyPrint newModule)
        let (newProject, _outcomes, _) = fromRight $ Actions.run testStdlib action
        newModules testStdlib newProject
          `shouldBe` 2

      it "Adds a new function to a new module that uses the Prelude" $ do
        let action = do
              -- add the Prelude
              (preludeHash', _mod') <- Actions.bindModule prelude "Prelude" (prettyPrint prelude)
              -- import Prelude
              let importExpr = unsafeParseModuleItem $ "import MyPrelude from " <> prettyPrint preludeHash'
              (mod2, _) <- Actions.addBindingToModule mempty mempty importExpr
              -- use Prelude
              let expr = unsafeParseModuleItem "def useFst = MyPrelude.fst (1,2)"
              (mod3, _) <- Actions.addBindingToModule mempty mod2 expr
              -- store the updated thing
              Actions.bindModule (getAnnotationForType <$> mod3) "Repl" (prettyPrint mod3)
        let (newProject, _outcomes, _) = fromRight $ Actions.run testStdlib action
        -- we've added two modules here
        newModules testStdlib newProject
          `shouldBe` 2

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
