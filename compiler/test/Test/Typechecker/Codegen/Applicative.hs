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
      it "pure for dtIdentity typechecks" $ do
        typecheckInstance applicativePure dtIdentity `shouldSatisfy` isRight

      it "Generates pure for dtIdentity" $ do
        applicativePure dtIdentity
          `shouldBe` Right
            ( unsafeParse
                "\\a -> Identity a"
            )

      it "pure for dtMaybe typechecks" $ do
        typecheckInstance applicativePure dtMaybe `shouldSatisfy` isRight

      it "Generates pure for dtMaybe" $ do
        applicativePure dtMaybe
          `shouldBe` Right
            ( unsafeParse
                "\\a -> Just a"
            )

      it "pure for dtTree typechecks" $ do
        typecheckInstance applicativePure dtTree `shouldSatisfy` isRight

      it "Generates pure for dtTree" $ do
        applicativePure dtTree
          `shouldBe` Right
            ( unsafeParse
                "\\a -> Leaf a"
            )

      it "pure for dtThese typechecks" $ do
        typecheckInstance applicativePure dtThese `shouldSatisfy` isRight

      it "Generates pure for dtThese" $ do
        applicativePure dtThese
          `shouldBe` Right
            ( unsafeParse
                "\\b -> That b"
            )

      it "pure for dtList typechecks" $ do
        typecheckInstance applicativePure dtList `shouldSatisfy` isRight

      it "Generates pure for dtList" $ do
        applicativePure dtList
          `shouldBe` Right
            ( unsafeParse
                "\\a -> Cons a Nil"
            )
      it "pure for dtMatchedPair typechecks" $ do
        typecheckInstance applicativePure dtMatchedPair `shouldSatisfy` isRight

      it "Generates pure for dtMatchedPair" $ do
        applicativePure dtMatchedPair
          `shouldBe` Right
            ( unsafeParse
                "\\a -> MatchedPair a a"
            )

      it "pure for dtReader typechecks" $ do
        typecheckInstance applicativePure dtReader `shouldSatisfy` isRight

      it "Generates pure for dtReader" $ do
        applicativePure dtReader
          `shouldBe` Right
            ( unsafeParse
                "\\a -> Reader \\r -> a"
            )

      it "Does not generate pure for dtEnv" $ do
        applicativePure dtEnv `shouldSatisfy` isLeft

    describe "apply instances" $ do
      it "apply for dtIdentity typechecks" $ do
        typecheckInstance applicativeApply dtIdentity `shouldSatisfy` isRight

      it "Generates apply for dtIdentity" $ do
        applicativeApply dtIdentity
          `shouldBe` Right
            ( unsafeParse $
                "\\identityF -> \\identityA -> "
                  <> "match identityF with (Identity f) -> "
                  <> "match identityA with (Identity a) -> Identity f(a)"
            )

      it "apply for dtMaybe typechecks" $ do
        typecheckInstance applicativeApply dtMaybe `shouldSatisfy` isRight

      it "Generates apply for dtMaybe" $ do
        applicativeApply dtMaybe
          `shouldBe` Right
            ( unsafeParse $
                "\\maybeF -> \\maybeA -> match maybeF with "
                  <> "(Just f) -> (match maybeA with (Just a) -> Just f(a) "
                  <> "| Nothing -> Nothing) "
                  <> "| Nothing -> Nothing"
            )

      it "apply for dtEither typechecks" $ do
        typecheckInstance applicativeApply dtEither `shouldSatisfy` isRight

      it "Generates apply for dtEither" $ do
        applicativeApply dtEither
          `shouldBe` Right
            ( unsafeParse $
                "\\eitherF -> \\eitherA -> match eitherF with "
                  <> "(Left a1) -> Left a1 | "
                  <> "(Right f) -> (match eitherA with (Left a1) -> Left a1 | (Right a) -> Right f(a))"
            )

      it "apply for dtPair typechecks" $ do
        typecheckInstance applicativeApply dtPair `shouldSatisfy` isRight

      it "Generates apply for dtPair" $ do
        applicativeApply dtPair
          `shouldBe` Right
            ( unsafeParse $
                "\\pairF -> \\pairA -> match pairF with "
                  <> "(Pair a f) -> (match pairA with (Pair a b) -> Pair a f(b))"
            )

      it "apply for dtThese typechecks" $ do
        typecheckInstance applicativeApply dtThese `shouldSatisfy` isRight

      it "Generates apply for dtThese" $ do
        applicativeApply dtThese
          `shouldBe` Right
            ( unsafeParse $
                "\\theseF -> \\theseA -> "
                  <> "match theseF with "
                  <> "(That f) -> (match theseA with "
                  <> "(That b) -> That f(b) | "
                  <> "(These a b) -> These a f(b) | "
                  <> "(This a1) -> This a1) | "
                  <> "(These a f) -> (match theseA with "
                  <> "(That b) -> That f(b) | "
                  <> "(These a b) -> These a f(b) | "
                  <> "(This a1) -> This a1) | "
                  <> "(This a1) -> This a1"
            )

      it "apply for dtEnv typechecks" $ do
        typecheckInstance applicativeApply dtEnv `shouldSatisfy` isRight

      it "Generates apply for dtEnv" $ do
        applicativeApply dtEnv
          `shouldBe` Right
            ( unsafeParse $
                "\\envF -> \\envA -> "
                  <> "match envF with (Env w f) -> "
                  <> "match envA with (Env w a) -> Env w f(a)"
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
