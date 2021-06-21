{-# LANGUAGE OverloadedStrings #-}

module Test.Codegen.Foldable
  ( spec,
  )
where

import Data.Either (isRight)
import Language.Mimsa.Codegen
import Test.Codegen.Shared
import Test.Hspec

spec :: Spec
spec = do
  describe "Foldable instances" $ do
    it "Foldable dtIdentity typechecks" $ do
      typecheckInstance fold dtIdentity `shouldSatisfy` isRight

    it "Generates fold for dtIdentity" $ do
      fold dtIdentity
        `shouldBe` Right
          ( unsafeParse
              "let fold = \\f -> \\total -> \\identity -> match identity with (Identity a) -> f(total)(a); fold"
          )

    it "Foldable dtMaybe typechecks" $ do
      typecheckInstance fold dtMaybe `shouldSatisfy` isRight

    it "Generates fold for dtMaybe" $ do
      fold dtMaybe
        `shouldBe` Right
          ( unsafeParse $
              "let fold = \\f -> \\total -> \\maybe -> match maybe with "
                <> "(Just a) -> f(total)(a) "
                <> "| Nothing -> total; fold"
          )

    it "Foldable dtThese typechecks" $ do
      typecheckInstance fold dtThese `shouldSatisfy` isRight

    it "Generates fold for dtThese" $ do
      fold dtThese
        `shouldBe` Right
          ( unsafeParse $
              "let fold = \\f -> \\total -> \\these -> match these with "
                <> "(That b) -> f(total)(b) | "
                <> "(These a b) -> f(total)(b) | "
                <> "(This a) -> total; "
                <> "fold"
          )
    it "Foldable dtList typechecks" $ do
      typecheckInstance fold dtList `shouldSatisfy` isRight

    it "Generates fold for dtList" $ do
      fold dtList
        `shouldBe` Right
          ( unsafeParse $
              "let fold = \\f -> \\total -> \\list -> match list with "
                <> "(Cons a list1) -> fold(f)(f(total)(a))(list1) | "
                <> "Nil -> total; "
                <> "fold"
          )

    it "Foldable dtEnv typechecks" $ do
      typecheckInstance fold dtEnv `shouldSatisfy` isRight

    it "Generates fold for dtEnv" $ do
      fold dtEnv
        `shouldBe` Right
          ( unsafeParse $
              "let fold = \\f -> \\total -> \\env -> "
                <> " match env with (Env w a) -> f(total)(a); "
                <> "fold"
          )
