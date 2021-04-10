{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Codegen.Foldable
  ( spec,
  )
where

import Data.Either (isRight)
import Language.Mimsa.Typechecker.Codegen
import Test.Hspec
import Test.Typechecker.Codegen.Shared

spec :: Spec
spec = do
  describe "Foldable instances" $ do
    it "Generates fold for dtIdentity" $ do
      typecheckInstance fold dtIdentity `shouldSatisfy` isRight
      fold dtIdentity
        `shouldBe` Right
          ( unsafeParse
              "let fold = \\f -> \\total -> \\identity -> case identity of Identity \\a -> f(total)(a); fold"
          )
    it "Generates fold for dtMaybe" $ do
      typecheckInstance fold dtMaybe `shouldSatisfy` isRight
      fold dtMaybe
        `shouldBe` Right
          ( unsafeParse
              "let fold = \\f -> \\total -> \\maybe -> case maybe of Just \\a -> f(total)(a) | Nothing total; fold"
          )
    it "Generates fold for dtThese" $ do
      typecheckInstance fold dtThese `shouldSatisfy` isRight
      fold dtThese
        `shouldBe` Right
          ( unsafeParse $
              "let fold = \\f -> \\total -> \\these -> case these of "
                <> "That \\b -> f(total)(b) | "
                <> "These \\a -> \\b -> f(total)(b) | "
                <> "This \\a -> total; "
                <> "fold"
          )
    it "Generates fold for dtList" $ do
      typecheckInstance fold dtList `shouldSatisfy` isRight
      fold dtList
        `shouldBe` Right
          ( unsafeParse $
              "let fold = \\f -> \\total -> \\list -> case list of "
                <> "Cons \\a -> \\list1 -> fold(f)(f(total)(a))(list1) | "
                <> "Nil total; "
                <> "fold"
          )
    it "Generates fold for dtEnv" $ do
      typecheckInstance fold dtEnv `shouldSatisfy` isRight
      fold dtEnv
        `shouldBe` Right
          ( unsafeParse $
              "let fold = \\f -> \\total -> \\env -> "
                <> " case env of Env \\w -> \\a -> f(total)(a); "
                <> "fold"
          )
