{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.Evaluate
  ( spec,
  )
where

import Data.Either (isRight)
import qualified Language.Mimsa.Actions.Modules.Bind as Actions
import qualified Language.Mimsa.Actions.Modules.Evaluate as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Types.Typechecker
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "Evaluate" $ do
      it "Should use the passed in module as context" $ do
        let action = do
              -- add a definition to an empty module
              let expr = unsafeParseModuleItem "def dog = True"
              (newMod, _) <- Actions.addBindingToModule mempty mempty expr
              -- evaluate using that module
              Actions.evaluateModule (unsafeParseExpr' "dog") (getAnnotationForType <$> newMod)
        Actions.run testStdlib action `shouldSatisfy` isRight
