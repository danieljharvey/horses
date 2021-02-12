{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Codegen
  ( spec,
  )
where

import Language.Mimsa.Typechecker.Codegen
import Test.Hspec
import qualified Test.Typechecker.Codegen.Enum as Enum
import qualified Test.Typechecker.Codegen.Foldable as Foldable
import qualified Test.Typechecker.Codegen.Functor as Functor
import qualified Test.Typechecker.Codegen.Newtype as Newtype
import Test.Typechecker.Codegen.Shared

spec :: Spec
spec = do
  describe "Codegen" $ do
    Foldable.spec
    Enum.spec
    Newtype.spec
    Functor.spec

  describe "typeclassMatches" $ do
    it "No instances for Void" $ do
      typeclassMatches dtVoid `shouldBe` mempty
    it "Enum instance for TrafficLights" $ do
      typeclassMatches dtTrafficLights `shouldSatisfy` elem Enum
    it "No enum instance for WrappedString" $ do
      typeclassMatches dtWrappedString `shouldNotSatisfy` elem Enum
    it "Newtype instance for WrappedString" $ do
      typeclassMatches dtWrappedString `shouldSatisfy` elem Newtype
    it "Functor instance for Identity" $ do
      typeclassMatches dtIdentity `shouldSatisfy` elem Functor
    it "Newtype instance for Identity" $ do
      typeclassMatches dtIdentity `shouldSatisfy` elem Newtype
    it "Functor instance for Maybe" $ do
      typeclassMatches dtMaybe `shouldSatisfy` elem Functor
    it "No newtype instance for Maybe" $ do
      typeclassMatches dtMaybe `shouldNotSatisfy` elem Newtype
