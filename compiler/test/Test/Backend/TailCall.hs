{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.TailCall
  ( spec,
  )
where

import Language.Mimsa.Backend.Core.TailCall
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers ()
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "TailCall" $ do
    describe "listReturns" $ do
      it "Simple value returns itself" $ do
        let expr = unsafeParseExpr "True"
        listReturns expr `shouldBe` [expr]
      it "Let returns it's body" $ do
        let expr = unsafeParseExpr "let a = 1 in True"
            expected = unsafeParseExpr "True"
        listReturns expr `shouldBe` [expected]
      it "Let pattern returns it's body" $ do
        let expr = unsafeParseExpr "let (a,b) = (1,2) in True"
            expected = unsafeParseExpr "True"
        listReturns expr `shouldBe` [expected]
      it "Data declaration returns body" $ do
        let expr = unsafeParseExpr "type Dog = Dog in True"
            expected = unsafeParseExpr "True"
        listReturns expr `shouldBe` [expected]
      it "Infix declaration returns body" $ do
        let expr = unsafeParseExpr "infix >>>>> = id in True"
            expected = unsafeParseExpr "True"
        listReturns expr `shouldBe` [expected]
      it "Pattern match returns all it's branches" $ do
        let expr = unsafeParseExpr "match True with True -> 1 | False -> 2"
            expected = [unsafeParseExpr "1", unsafeParseExpr "2"]
        listReturns expr `shouldBe` expected
      it "If returns both branches" $ do
        let expr = unsafeParseExpr "if True then 1 else 2"
            expected = [unsafeParseExpr "1", unsafeParseExpr "2"]
        listReturns expr `shouldBe` expected
      it "Array returns itself" $ do
        let expr = unsafeParseExpr "[1,2]"
        listReturns expr `shouldBe` [expr]
      it "Application returns itself" $ do
        let expr = unsafeParseExpr "fn 1 2"
        listReturns expr `shouldBe` [expr]
      it "Lambdas return result" $ do
        let expr = unsafeParseExpr "\\a -> \\b -> 1"
            expected = unsafeParseExpr "1"
        listReturns expr `shouldBe` [expected]
    describe "isTailRecursive" $ do
      it "Detects a let with no recursion" $ do
        let expr = unsafeParseExpr "True"
            result = isTailRecursive expr "fn" mempty
        result `shouldBe` NoRecursion
      it "Detects a let with no tail recursion" $ do
        let expr =
              unsafeParseExprWithAnn
                "\\a -> \\value -> \\count -> if count == 0 then value else (1 + fn (value) (count - 1))"
            result = isTailRecursive expr "fn" mempty
        result
          `shouldBe` Recursion
            mempty
            "fn"
            [ NonTailRecursion (Location 57 83)
            ]
      it "Detects a let with simple tail recursion" $ do
        let expr =
              unsafeParseExprWithAnn
                "\\a -> \\value -> \\count -> if count == 0 then value else (fn (value + 1) (count - 1))"
            result = isTailRecursive expr "fn" mempty
        result
          `shouldBe` Recursion
            mempty
            "fn"
            [ TailRecursion (Location 56 84)
            ]
