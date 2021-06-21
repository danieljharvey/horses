{-# LANGUAGE OverloadedStrings #-}

module Test.Codegen.Functor
  ( spec,
  )
where

import Data.Either (isRight)
import Language.Mimsa.Codegen
import Test.Codegen.Shared
import Test.Hspec

spec :: Spec
spec = do
  describe "Functor instances" $ do
    it "dtIdentity functor typechecks" $ do
      typecheckInstance functorMap dtIdentity `shouldSatisfy` isRight

    it "Generates functorMap for dtIdentity" $ do
      functorMap dtIdentity
        `shouldBe` Right
          ( unsafeParse $
              "let fmap = \\f -> \\identity -> match identity with "
                <> " (Identity a) -> Identity f(a); "
                <> "fmap"
          )
    it "dtMaybe functor typechecks" $ do
      typecheckInstance functorMap dtMaybe `shouldSatisfy` isRight

    it "Generates functorMap for dtMaybe" $ do
      functorMap dtMaybe
        `shouldBe` Right
          ( unsafeParse $
              "let fmap = \\f -> \\maybe -> match maybe with "
                <> "(Just a) -> Just f(a) | Nothing -> Nothing; fmap"
          )

    it "dtThese functor typechecks" $ do
      typecheckInstance functorMap dtThese `shouldSatisfy` isRight

    it "Generates functorMap for dtThese" $ do
      functorMap dtThese
        `shouldBe` Right
          ( unsafeParse $
              "let fmap = \\f -> \\these -> match these with "
                <> "(That b) -> That f(b) | "
                <> "(These a b) -> These a f(b) | "
                <> "(This a) -> This a; "
                <> "fmap"
          )

    it "dtList functor typechecks" $ do
      typecheckInstance functorMap dtList `shouldSatisfy` isRight

    it "Generates functorMap for dtList" $ do
      functorMap dtList
        `shouldBe` Right
          ( unsafeParse $
              "let fmap = \\f -> \\list -> match list with "
                <> "(Cons a list1) -> Cons f(a) fmap(f)(list1) | "
                <> "Nil -> Nil; "
                <> "fmap"
          )

    it "dtDoubleList functor typechecks" $ do
      typecheckInstance functorMap dtDoubleList `shouldSatisfy` isRight

    it "Generates functorMap for dtDoubleList" $ do
      functorMap dtDoubleList
        `shouldBe` Right
          ( unsafeParse $
              "let fmap = \\f -> \\doubleList -> match doubleList with "
                <> "(DoubleCons a b doubleList1) -> DoubleCons a f(b) fmap(f)(doubleList1) | "
                <> "DoubleNil -> DoubleNil; "
                <> "fmap"
          )

    it "dtTree functor typechecks" $ do
      typecheckInstance functorMap dtTree `shouldSatisfy` isRight

    it "Generates functorMap for dtTree" $ do
      functorMap dtTree
        `shouldBe` Right
          ( unsafeParse $
              "let fmap = \\f -> \\tree -> match tree with "
                <> "(Branch tree1 tree2) -> Branch fmap(f)(tree1) fmap(f)(tree2) | "
                <> "(Leaf a) -> Leaf f(a); "
                <> "fmap"
          )

    it "dtReader functor typechecks" $ do
      typecheckInstance functorMap dtReader `shouldSatisfy` isRight

    it "Generates functorMap for dtReader" $ do
      functorMap dtReader
        `shouldBe` Right
          ( unsafeParse $
              "let fmap = \\f -> \\reader -> match reader with "
                <> "(Reader rtoa) -> Reader \\r -> f(rtoa(r)); "
                <> "fmap"
          )

    it "dtEnv functor typechecks" $ do
      typecheckInstance functorMap dtEnv `shouldSatisfy` isRight

    it "Generates functorMap for dtEnv" $ do
      functorMap dtEnv
        `shouldBe` Right
          ( unsafeParse $
              "let fmap = \\f -> \\env -> match env with "
                <> "(Env w a) -> Env w f(a); "
                <> "fmap"
          )
