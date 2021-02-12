{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Codegen.Functor
  ( spec,
  )
where

import Data.Either (isRight)
import Language.Mimsa.Typechecker.Codegen
import Test.Hspec
import Test.Typechecker.Codegen.Shared

spec :: Spec
spec = do
  describe "Functor instances" $ do
    it "Generates functorMap for dtIdentity" $ do
      typecheckInstance functorMap dtIdentity `shouldSatisfy` isRight
      functorMap dtIdentity
        `shouldBe` Right
          ( unsafeParse $
              "let fmap = \\f -> \\identity -> case identity of "
                <> " Identity \\a -> Identity f(a); "
                <> "fmap"
          )
    it "Generates functorMap for dtMaybe" $ do
      typecheckInstance functorMap dtMaybe `shouldSatisfy` isRight
      functorMap dtMaybe
        `shouldBe` Right
          ( unsafeParse $
              "let fmap = \\f -> \\maybe -> case maybe of "
                <> "Just \\a -> Just f(a) | Nothing Nothing; fmap"
          )
    it "Generates functorMap for dtThese" $ do
      typecheckInstance functorMap dtThese `shouldSatisfy` isRight
      functorMap dtThese
        `shouldBe` Right
          ( unsafeParse $
              "let fmap = \\f -> \\these -> case these of "
                <> "That \\b -> That f(b) |"
                <> "These \\a -> \\b -> These a f(b) | "
                <> "This \\a -> This a; "
                <> "fmap"
          )
    it "Generates functorMap for dtList" $ do
      typecheckInstance functorMap dtList `shouldSatisfy` isRight
      functorMap dtList
        `shouldBe` Right
          ( unsafeParse $
              "let fmap = \\f -> \\list -> case list of "
                <> "Cons \\a -> \\list1 -> Cons f(a) fmap(f)(list1) |"
                <> "Nil Nil; "
                <> "fmap"
          )
    it "Generates functorMap for dtDoubleList" $ do
      typecheckInstance functorMap dtDoubleList `shouldSatisfy` isRight
      functorMap dtDoubleList
        `shouldBe` Right
          ( unsafeParse $
              "let fmap = \\f -> \\doubleList -> case doubleList of "
                <> "DoubleCons \\a -> \\b -> \\doubleList1 -> DoubleCons a f(b) fmap(f)(doubleList1) | "
                <> "DoubleNil DoubleNil; "
                <> "fmap"
          )
    it "Generates functorMap for dtTree" $ do
      typecheckInstance functorMap dtTree `shouldSatisfy` isRight
      functorMap dtTree
        `shouldBe` Right
          ( unsafeParse $
              "let fmap = \\f -> \\tree -> case tree of "
                <> "Branch \\tree1 -> \\tree2 -> Branch fmap(f)(tree1) fmap(f)(tree2) | "
                <> "Leaf \\a -> Leaf f(a); "
                <> "fmap"
          )
    it "Generates functorMap for dtReader" $ do
      typecheckInstance functorMap dtReader `shouldSatisfy` isRight
      functorMap dtReader
        `shouldBe` Right
          ( unsafeParse $
              "let fmap = \\f -> \\reader -> case reader of "
                <> "Reader \\rtoa -> Reader \\r -> f(rtoa(r)); "
                <> "fmap"
          )
