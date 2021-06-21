{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Codegen.Newtype
  ( spec,
  )
where

import Data.Either (isRight)
import Language.Mimsa.Typechecker.Codegen
import Test.Hspec
import Test.Typechecker.Codegen.Shared

spec :: Spec
spec = do
  describe "Newtype instances" $ do
    it "Newtype wrap dtWrappedString typechecks" $ do
      typecheckInstance wrap dtWrappedString `shouldSatisfy` isRight

    it "Generates wrap for dtWrappedString" $ do
      wrap dtWrappedString
        `shouldBe` Right
          (unsafeParse "\\a -> Wrapped a")

    it "Newtype unwrap dtWrappedString typechecks" $ do
      typecheckInstance unwrap dtWrappedString `shouldSatisfy` isRight

    it "Generates unwrap for dtWrappedString" $ do
      unwrap dtWrappedString
        `shouldBe` Right
          (unsafeParse "\\wrappedString -> match wrappedString with (Wrapped a) -> a")
