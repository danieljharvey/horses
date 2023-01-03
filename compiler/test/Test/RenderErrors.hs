{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.RenderErrors (spec) where

-- this prints out loads of errors so we can look at them and decide if they
-- look bad

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Error.Diagnose hiding (Annotation)
import qualified Language.Mimsa.Actions.Helpers.Parse as Actions
import qualified Language.Mimsa.Actions.Modules.Evaluate as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Core
import Language.Mimsa.Project.Stdlib (buildStdlib)
import Language.Mimsa.Core
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Test.Hspec

stdlib :: Project Annotation
stdlib = case buildStdlib of
  Right a -> a
  _ -> error "Error building stdlib in error printing tests"

printError ::
  Project Annotation ->
  Text ->
  Expectation
printError env input = do
  let action = do
        expr <- Actions.parseExpr input
        (mt, interpretedExpr, _) <- Actions.evaluateModule expr mempty
        pure (mt, interpretedExpr)
  case Actions.run env action of
    Right (_, _, result) -> do
      liftIO $
        putStrLn
          ("Expected error, got " <> T.unpack (prettyPrint result))
      False `shouldBe` True
    Left err' -> do
      let diag = errorToDiagnostic err'
      liftIO (T.putStrLn "\n---")
      liftIO (T.putStrLn input)
      liftIO $ printDiagnostic stderr True True 4 defaultStyle diag
      liftIO (T.putStrLn "---\n")
      True `shouldBe` True

spec :: Spec
spec =
  describe "Error printing" $ do
    describe "Store errors" $ do
      it "Cannot find dependency" $ do
        printError stdlib "mysteryFunction (1,2)"
    describe "Type errors" $ do
      it "Non-boolean in If predicate" $ do
        printError stdlib "if 1 then True else False"
      it "Non-boolean in If predicate with annotation" $ do
        printError stdlib "let (a: Boolean) = if 1 then True else False; 1"
      it "Non-boolean from function in if predicate" $ do
        printError stdlib "let f a = if a then 1 else 2; f 1"
      it "Non-matching if branches" $ do
        printError stdlib "if True then 1 else False"
      it "Patterns do not match input" $ do
        printError stdlib "match Just True with Right a -> a | _ -> False"
      it "Pattern match branches have different types" $ do
        printError stdlib "match Just True with Just a -> a | _ -> 100"
      it "Defines a type twice" $ do
        printError stdlib "type Dog = Dog; type Dog = Log; True"
      it "Pattern match with no matches" $ do
        printError stdlib "match True with"
      it "Type constructor uses variable not found in type" $ do
        printError stdlib "type Maybe a = Just b; True"
      it "Uses built-in type as constructor in type definition" $ do
        printError stdlib "type Dog = String Int; True"
      it "Annotated function called with wrong type argument" $ do
        printError stdlib "let (f: Int -> Boolean) i = True; f False"
      it "Inferred function called with wrong type argument" $ do
        printError stdlib "let f i = i + 1; f False"
      it "Applied a value to non-function" $ do
        printError stdlib "let f = 1; f True"
      it "Applied wrong value to lambda" $ do
        printError stdlib "(\\a -> a + 1) True"
      it "Applies two args to single arity func" $ do
        printError stdlib "let f a = a + 1; f 1 True"
