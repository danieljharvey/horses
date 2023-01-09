{-# LANGUAGE OverloadedStrings #-}

module Test.Transform.FlattenPatternSpec (spec) where

import Control.Monad.Except
import Control.Monad.Reader
import Test.Helpers
import Test.Hspec
import Smol.Core

flattenPatterns' ::
  (Monoid ann, Show ann) =>
  Expr ann ->
  Either (TCError ann) (Expr ann)
flattenPatterns' patterns =
  let env = TCEnv mempty mempty builtInTypes
   in runExcept $ runReaderT (flattenPatterns patterns) env

-- this is all so fucked but want to hold on to it

spec :: Spec
spec = do
  describe "FlattenPatternMatch" $ do
    it "Is already flat, do nothing" $ do
      let input = unsafeParseExpr "case input of a -> a"
      flattenPatterns' input `shouldBe` Right input

    xit "Non-nested pattern, also do nothing" $ do
      let input = unsafeParseExpr "case input of Just a -> a | _ -> 0"
      flattenPatterns' input `shouldBe` Right input

    it "Nested patterns are split into two matches" $ do
      let input = unsafeParseExpr "case input of Just (Just a) -> a | Just Nothing -> 1 | Nothing -> 0"
          expected = unsafeParseExpr "case input of Just v1 -> (case v1 of Just a -> a | Nothing -> 1) | Nothing -> 0"

      flattenPatterns' input `shouldBe` Right expected

    xit "Nested patterns with wildcards are split into two matches with wildcards in both" $ do
      let input = unsafeParseExpr "case input of Just (Just a) -> a | _ -> 0"
          expected = unsafeParseExpr "case input of Just v1 -> (case v1 of Just a -> a | _ -> 0) | Nothing -> 0"

      flattenPatterns' input `shouldBe` Right expected

    xit "Nested with two items inside" $ do
      let input = unsafeParseExpr "case input of This (This a) -> a | _ -> 0"
          expected = unsafeParseExpr "case input of That _ -> 0 | These _ _ -> 0 | This v1 -> (case v1 of This a -> a | _ -> 0)"

      flattenPatterns' input `shouldBe` Right expected

    xit "Doubly nested patterns are split into three layers" $ do
      let input = unsafeParseExpr "case input of Just (Just (Just a)) -> a | _ -> 0"
          expected = unsafeParseExpr "case input of Just v1 -> (case v1 of Just v2 -> (case v2 of Just a -> a | Nothing -> 0) | Nothing -> 0) | Nothing -> 0"

      flattenPatterns' input `shouldBe` Right expected
