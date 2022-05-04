{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

-- this prints out loads of errors so we can look at them and decide if they
-- look bad

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Error.Diagnose
import qualified Language.Mimsa.Actions.Evaluate as Actions
import qualified Language.Mimsa.Actions.Helpers.Parse as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Stdlib (buildStdlib)
import Language.Mimsa.Types.AST
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
        (mt, interpretedExpr, storeExpr, _, _) <- Actions.evaluate input expr
        pure (mt, interpretedExpr, storeExpr)
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
      liftIO $ printDiagnostic stderr True True 4 diag
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
      it "Non-matching if branches" $ do
        printError stdlib "if True then 1 else False"
      it "Patterns do not match input" $ do
        printError stdlib "match Just True with Right a -> a | _ -> False"
      it "Pattern match branches have different types" $ do
        printError stdlib "match Just True with Just a -> a | _ -> 100"

main :: IO ()
main = hspec spec
