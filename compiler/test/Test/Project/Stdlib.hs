{-# LANGUAGE OverloadedStrings #-}

module Test.Project.Stdlib
  ( spec,
  )
where

import Control.Monad.IO.Class
import Data.Coerce
import Data.Either
import Data.Foldable
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Interpret as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Test.Hspec
import Test.Utils.Helpers

evaluatesSuccessfully ::
  Project Annotation ->
  Text ->
  Spec
evaluatesSuccessfully env input =
  it ("Evaluates " <> T.unpack input <> " from stdlib") $ do
    case evaluateText env input of
      Left e -> error (T.unpack (prettyPrint e))
      Right resolved -> do
        Actions.run env (Actions.interpreter (reStoreExpression resolved))
          `shouldSatisfy` isRight

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
