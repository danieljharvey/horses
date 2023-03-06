{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.Compile
  ( spec,
  )
where

import Data.Either (isRight)
import Data.Foldable
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Compile as Actions
import qualified Language.Mimsa.Actions.Helpers.LookupExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Backend.Types
import Language.Mimsa.Core
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Types.Project
import Test.Hspec

spec :: Spec
spec = do
  describe "Compile" $ do
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
