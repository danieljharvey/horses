{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Codegen.Applicative
  ( spec,
  )
where

import Data.Either (isRight)
import Language.Mimsa.Typechecker.Codegen
import Test.Hspec
import Test.Typechecker.Codegen.Shared

spec :: Spec
spec = do
  describe "Applicative instances" $ do
    it "Generates pure for dtIdentity" $ do
      typecheckInstance applicativePure dtIdentity `shouldSatisfy` isRight
      applicativePure dtIdentity
        `shouldBe` Right
          ( unsafeParse
              "\\a -> Identity a"
          )
    it "Generates pure for dtMaybe" $ do
      typecheckInstance applicativePure dtMaybe `shouldSatisfy` isRight
      applicativePure dtMaybe
        `shouldBe` Right
          ( unsafeParse
              "\\a -> Just a"
          )
    it "Generates pure for dtTree" $ do
      typecheckInstance applicativePure dtTree `shouldSatisfy` isRight
      applicativePure dtTree
        `shouldBe` Right
          ( unsafeParse
              "\\a -> Leaf a"
          )
    it "Generates pure for dtThese" $ do
      typecheckInstance applicativePure dtThese `shouldSatisfy` isRight
      applicativePure dtThese
        `shouldBe` Right
          ( unsafeParse
              "\\b -> That b"
          )
    it "Generates pure for dtList" $ do
      typecheckInstance applicativePure dtList `shouldSatisfy` isRight
      applicativePure dtList
        `shouldBe` Right
          ( unsafeParse
              "\\a -> Cons a Nil"
          )
    it "Generates pure for dtMatchedPair" $ do
      typecheckInstance applicativePure dtMatchedPair `shouldSatisfy` isRight
      applicativePure dtMatchedPair
        `shouldBe` Right
          ( unsafeParse
              "\\a -> MatchedPair a a"
          )
    it "Generates pure for dtReader" $ do
      typecheckInstance applicativePure dtReader `shouldSatisfy` isRight
      applicativePure dtReader
        `shouldBe` Right
          ( unsafeParse
              "\\a -> Reader \\r -> a"
          )
