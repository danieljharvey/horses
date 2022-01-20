{-# LANGUAGE OverloadedStrings #-}

module Test.ErrorFormat
  ( spec,
  )
where

import Language.Mimsa.ErrorFormat
import Language.Mimsa.Types.AST.Annotation (Annotation (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "ErrorFormat" $ do
    it "Just message is message" $ do
      let msg = "Great job"
      printDiagnostic (toDiagnostic msg mempty "") `shouldBe` msg
    it "Print message and mentioned line" $ do
      let msg = "Oh no"
          input = "let a = True"
          sourceItem = ("Should be false", Location 9 12)
          expected = "Oh no\n1. let a = True\n        ^^^^\n        Should be false"
      printDiagnostic (toDiagnostic msg [sourceItem] input) `shouldBe` expected
