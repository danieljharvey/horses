{-# LANGUAGE OverloadedStrings #-}

module Test.Typecheck.NestingMonadSpec (spec) where

import Control.Monad.Writer.Strict
import Test.Hspec

spec :: Spec
spec = do
  describe "Nesting" $ do
    it "Writes some stuff" $ do
      let action = tell [1 :: Int, 2, 3]
      runWriter action
        `shouldBe` ((), [1, 2, 3])

    it "Writes some stuff in a sub-action" $ do
      let subAction = tell [4 :: Int, 5, 6]
          action = do
            tell [1 :: Int, 2, 3]
            subAction

      runWriter action
        `shouldBe` ((), [1, 2, 3, 4, 5, 6])

    it "Writes some stuff in a sub-action, throws it away" $ do
      let subAction = tell [4 :: Int, 5, 6]
          action = do
            tell [1 :: Int, 2, 3]
            let (_, as) = runWriter subAction
            tell [7, 8, 9]
            pure as

      runWriter action
        `shouldBe` ([4, 5, 6], [1, 2, 3, 7, 8, 9])
