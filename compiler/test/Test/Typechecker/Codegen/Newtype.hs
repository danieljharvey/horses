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
    it "Generates wrap for dtWrappedString" $ do
      typecheckInstance wrap dtWrappedString `shouldSatisfy` isRight
      wrap dtWrappedString
        `shouldBe` Right
          (unsafeParse "\\a -> Wrapped a")
    it "Generates unwrap for dtWrappedString" $ do
      typecheckInstance unwrap dtWrappedString `shouldSatisfy` isRight
      unwrap dtWrappedString
        `shouldBe` Right
          (unsafeParse "\\wrappedString -> match wrappedString with (Wrapped a) -> a")
