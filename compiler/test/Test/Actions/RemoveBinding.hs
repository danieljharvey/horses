{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.RemoveBinding
  ( spec,
  )
where

import Data.Either (isLeft)
import Data.Maybe (isNothing)
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.RemoveBinding as Actions
import Language.Mimsa.Project.Helpers
import Test.Data.Project
import Test.Hspec

spec :: Spec
spec = do
  describe "RemoveBinding" $ do
    it "Fails when removing a non-existing item" $ do
      Actions.run
        testStdlib
        ( Actions.removeBinding
            "madeUpNameThatIsntThere"
        )
        `shouldSatisfy` isLeft
    it "Successfully removes a binding" $ do
      case Actions.run testStdlib (Actions.removeBinding "execState") of
        Left _ -> error "Should not have failed"
        Right (newProject, _, _) -> do
          -- no `execState` binding
          lookupBindingName
            newProject
            "execState"
            `shouldSatisfy` isNothing
