{-# LANGUAGE OverloadedStrings #-}

module Test.Modules.Test
  ( spec,
  )
where

import Data.Either (isLeft)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Modules.Check as Actions
import qualified Language.Mimsa.Actions.Modules.RunTests as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Tests
import Test.Hspec

joinLines :: [Text] -> Text
joinLines = T.intercalate "\n"

runTests :: Text -> Either (Error Annotation) (Map TestName ModuleTestResult)
runTests t = do
  let action = do
        (modA, _tyA) <- Actions.checkModule (prjModuleStore stdlib) t
        Actions.runModuleTests modA
  (_, _, a) <- Actions.run stdlib action
  pure a

spec :: Spec
spec = do
  describe "Modules tests" $ do
    it "Trivial passing unit test" $
      runTests
        (joinLines ["test \"2 equals 2\" = 2 == 2"])
        `shouldBe` Right (M.singleton (TestName "2 equals 2") ModuleTestPassed)

    it "Trivial failing unit test" $
      runTests
        (joinLines ["test \"2 equals 3\" = 2 == 3"])
        `shouldBe` Right (M.singleton (TestName "2 equals 3") ModuleTestFailed)

    it "A test that does not typecheck with Boolean fails" $
      runTests
        (joinLines ["test \"2 equals 2\" = 2"])
        `shouldSatisfy` isLeft

    it "Passing unit test using module functions" $
      runTests
        ( joinLines
            [ "test \"identity 2 equals 2\" = id 2 == 2",
              "def id a = a"
            ]
        )
        `shouldBe` Right (M.singleton (TestName "identity 2 equals 2") ModuleTestPassed)

    it "Runs a trivial test that refers to a Prelude expression" $ do
      let preludeHash = fromJust (M.lookup "Prelude" (getCurrentModules $ prjModules stdlib))
       in runTests
            ( joinLines
                [ "test \"Prelude.id 2 equals 2\" = Prelude.id 2 == 2",
                  "import Prelude from " <> prettyPrint preludeHash
                ]
            )
            `shouldBe` Right (M.singleton (TestName "Prelude.id 2 equals 2") ModuleTestPassed)
