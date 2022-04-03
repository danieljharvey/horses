{-# LANGUAGE OverloadedStrings #-}

module Test.Transform.FlattenLets
  ( spec,
  )
where

import Language.Mimsa.Transform.FlattenLets
import Language.Mimsa.Types.AST
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "FlattenLets" $ do
    describe "Remove nested lets" $ do
      it "Does nothing when no nested lets" $ do
        let expr = unsafeParseExpr "let a = True in 1"
        flattenLets expr `shouldBe` expr
      it "Flattens a let" $ do
        let expr = unsafeParseExpr "let a = (let b = 1 in True) in a + 1"
            expected = unsafeParseExpr "let b = 1; let a = True; a + 1"
        flattenLets expr `shouldBe` expected
      it "Flattens many lets" $ do
        let expr = unsafeParseExpr "let a = (let b = (let c = 1 in c) in True) in a + 1"
            expected = unsafeParseExpr "let c = 1; let b = c; let a = True; a + 1"
        flattenLets expr `shouldBe` expected
    describe "Simple let patterns become Let again" $ do
      it "Turns a simple let pattern into let" $ do
        let expr = MyLetPattern () (PVar mempty ("a" :: String)) (int 1) (MyVar mempty "a")
            expected = MyLet mempty (Identifier mempty "a") (int 1) (MyVar mempty "a")
        flattenLets expr `shouldBe` expected
    describe "Single match pattern matches become let patterns" $ do
      it "Leaves a multiple pattern match alone" $ do
        let expr = unsafeParseExpr "match [1,2,3] with [] -> True | other -> False"
        flattenLets expr `shouldBe` expr
      it "Converts a single match into a Let Pattern" $ do
        let expr = unsafeParseExpr "match (Identity 1) with (Identity a) -> a"
            expected = unsafeParseExpr "let (Identity a) = Identity 1 in a"
        flattenLets expr `shouldBe` expected
    describe "Remove unbound let" $ do
      it "Removes let pattern with wildcard" $ do
        let expr = unsafeParseExpr "let _ = 1 in 2"
            expected = unsafeParseExpr "2"
        flattenLets expr `shouldBe` expected
