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
    describe "pure instances" $ do
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
    describe "apply instances" $ do
      it "Generates apply for dtIdentity" $ do
        typecheckInstance applicativeApply dtIdentity `shouldSatisfy` isRight
        applicativeApply dtIdentity
          `shouldBe` Right
            ( unsafeParse
                "\\identityF -> \\identityA -> case identityF of Identity \\f -> case identityA of Identity \\a -> Identity f(a)"
            )
      it "Generates apply for dtMaybe" $ do
        -- typecheckInstance applicativeApply dtMaybe `shouldSatisfy` isRight
        applicativeApply dtMaybe
          `shouldBe` Right
            ( unsafeParse
                "\\maybeF -> \\maybeA -> case maybeF of Just \\f -> (case maybeA of Just \\a -> Just f(a) | Nothing Nothing ) | Nothing Nothing"
            )
      xit "Generates apply for dtTree" $ do
        typecheckInstance applicativeApply dtTree `shouldSatisfy` isRight
        applicativeApply dtTree
          `shouldBe` Right
            ( unsafeParse
                "\\a -> Leaf a"
            )
      xit "Generates apply for dtThese" $ do
        typecheckInstance applicativeApply dtThese `shouldSatisfy` isRight
        applicativeApply dtThese
          `shouldBe` Right
            ( unsafeParse
                "\\b -> That b"
            )
      xit "Generates apply for dtList" $ do
        typecheckInstance applicativeApply dtList `shouldSatisfy` isRight
        applicativeApply dtList
          `shouldBe` Right
            ( unsafeParse
                "\\a -> Cons a Nil"
            )
      xit "Generates apply for dtMatchedPair" $ do
        typecheckInstance applicativeApply dtMatchedPair `shouldSatisfy` isRight
        applicativeApply dtMatchedPair
          `shouldBe` Right
            ( unsafeParse
                "\\a -> MatchedPair a a"
            )
      xit "Generates apply for dtReader" $ do
        typecheckInstance applicativeApply dtReader `shouldSatisfy` isRight
        applicativeApply dtReader
          `shouldBe` Right
            ( unsafeParse
                "\\a -> Reader \\r -> a"
            )
