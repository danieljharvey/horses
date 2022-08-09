{-# LANGUAGE OverloadedStrings #-}

module Test.Project.Stdlib
  ( spec,
  )
where

import Control.Monad.IO.Class
import Data.Coerce
import Data.Either
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Helpers.LookupExpression as Actions
import qualified Language.Mimsa.Actions.Interpret as Actions
import qualified Language.Mimsa.Actions.Modules.Check as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Test.Hspec
import Test.Utils.Helpers

evaluatesSuccessfully ::
  Project Annotation ->
  Text ->
  Spec
evaluatesSuccessfully project input =
  it ("Evaluates " <> T.unpack input <> " from stdlib") $ do
    case evaluateText project input of
      Left e -> error (T.unpack (prettyPrint e))
      Right resolved -> do
        Actions.run project (Actions.interpreter (reStoreExpression resolved))
          `shouldSatisfy` isRight

moduleTypechecksSuccessfully ::
  Project Annotation ->
  (ModuleName, ModuleHash) ->
  Spec
moduleTypechecksSuccessfully project (modName, modHash) =
  it ("Typechecks module " <> T.unpack (prettyPrint modName) <> " from stdlib") $ do
    let action = do
          thisMod <- Actions.lookupModule modHash
          Actions.checkModule (prjModuleStore project) (prettyPrint thisMod)
    Actions.run project action `shouldSatisfy` isRight

spec :: Spec
spec = do
  describe "Stdlib" $ do
    it "Builds" $ do
      buildStdlib `shouldSatisfy` isRight

  describe "Can evaluate each top-level item" $ do
    case buildStdlib of
      Right prj ->
        let bindingNames = M.keys . getVersionedMap . prjBindings $ prj
         in traverse_ (evaluatesSuccessfully prj . coerce) bindingNames
      Left e ->
        it "could not create stdlib" $ do
          liftIO (putStrLn (T.unpack (prettyPrint e)))
          False `shouldBe` True

  describe "Can typecheck each top-level module" $ do
    case buildStdlib of
      Right prj ->
        let moduleNames = M.toList . getCurrentModules . prjModules $ prj
         in traverse_ (moduleTypechecksSuccessfully prj) moduleNames
      Left e ->
        it "could not create stdlib" $ do
          liftIO (putStrLn (T.unpack (prettyPrint e)))
          False `shouldBe` True
