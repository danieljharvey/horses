{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Codegen.Applicative
  ( spec,
  )
where

import Data.Either (isLeft, isRight)
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
      it "Does not generate pure for dtEnv" $ do
        applicativePure dtEnv `shouldSatisfy` isLeft

    describe "apply instances" $ do
      it "Generates apply for dtIdentity" $ do
        typecheckInstance applicativeApply dtIdentity `shouldSatisfy` isRight
        applicativeApply dtIdentity
          `shouldBe` Right
            ( unsafeParse
                "\\identityF -> \\identityA -> case identityF of Identity \\f -> case identityA of Identity \\a -> Identity f(a)"
            )
      it "Generates apply for dtMaybe" $ do
        typecheckInstance applicativeApply dtMaybe `shouldSatisfy` isRight
        applicativeApply dtMaybe
          `shouldBe` Right
            ( unsafeParse $
                "\\maybeF -> \\maybeA -> case maybeF of "
                  <> "Just \\f -> (case maybeA of Just \\a -> Just f(a) "
                  <> "| Nothing Nothing ) "
                  <> "| Nothing Nothing"
            )
      it "Generates apply for dtEither" $ do
        typecheckInstance applicativeApply dtEither `shouldSatisfy` isRight
        applicativeApply dtEither
          `shouldBe` Right
            ( unsafeParse $
                "\\eitherF -> \\eitherA -> case eitherF of "
                  <> "Left \\a1 -> Left a1 | "
                  <> "Right \\f -> (case eitherA of Left \\a1 -> Left a1 | Right \\a -> Right f(a))"
            )
      it "Generates apply for dtPair" $ do
        typecheckInstance applicativeApply dtPair `shouldSatisfy` isRight
        applicativeApply dtPair
          `shouldBe` Right
            ( unsafeParse $
                "\\pairF -> \\pairA -> case pairF of "
                  <> "Pair \\a -> \\f -> (case pairA of Pair \\a -> \\b -> Pair a f(b))"
            )

      it "Generates apply for dtThese" $ do
        typecheckInstance applicativeApply dtThese `shouldSatisfy` isRight
        applicativeApply dtThese
          `shouldBe` Right
            ( unsafeParse $
                "\\theseF -> \\theseA -> "
                  <> "case theseF of "
                  <> "That \\f -> (case theseA of "
                  <> "That \\b -> That f(b) | "
                  <> "These \\a -> \\b -> These a f(b) | "
                  <> "This \\a1 -> This a1) | "
                  <> "These \\a -> \\f -> (case theseA of "
                  <> "That \\b -> That f(b) | "
                  <> "These \\a -> \\b -> These a f(b) | "
                  <> "This \\a1 -> This a1) | "
                  <> "This \\a1 -> This a1"
            )
      it "Generates apply for dtEnv" $ do
        typecheckInstance applicativeApply dtEnv `shouldSatisfy` isRight
        applicativeApply dtEnv
          `shouldBe` Right
            ( unsafeParse $
                "\\envF -> \\envA -> "
                  <> "case envF of Env \\w -> \\f -> "
                  <> "case envA of Env \\w -> \\a -> Env w f(a)"
            )

      it "Does not make apply for dtConsoleF" $ do
        applicativeApply dtConsoleF `shouldSatisfy` isLeft
      it "Does not make apply for dtReader" $ do
        applicativeApply dtReader `shouldSatisfy` isLeft
      it "Does not make apply for dtMatchedPair" $ do
        applicativeApply dtMatchedPair `shouldSatisfy` isLeft
      it "Does not make apply for dtList" $ do
        applicativeApply dtList `shouldSatisfy` isLeft
      it "Does not make apply for dtTree" $ do
        applicativeApply dtTree `shouldSatisfy` isLeft
