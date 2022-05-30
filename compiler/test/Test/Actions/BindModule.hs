{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.BindModule
  ( spec,
  )
where

import Language.Mimsa.Modules.Prelude
import Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.BindModule as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.Project
import Test.Data.Project
import Test.Hspec

newModules :: Project ann -> Project ann -> Int
newModules old new = 
  let countModules = length .  prjModuleStore
  in countModules new - countModules old

spec :: Spec
spec = do
  describe "BindModule" $ do
    it "Adds a fresh new module to prjModules and to Store" $ do
      case Actions.run testStdlib (Actions.bindModule prelude "Prelude" (prettyPrint prelude)) of
        Left _ -> error "Should not have failed"
        Right (newProject, outcomes, _) -> do
          -- one more item in module store
          newModules testStdlib newProject 
            `shouldBe` 1 
          -- one more binding
          lookupModuleName
            newProject
            "Prelude"
            `shouldSatisfy` isJust
          -- one new store expression
          S.size (Actions.modulesFromOutcomes outcomes)
            `shouldBe` 1

