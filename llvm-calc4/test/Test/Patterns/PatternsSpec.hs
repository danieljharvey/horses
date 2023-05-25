{-# LANGUAGE OverloadedStrings #-}

module Test.Patterns.PatternsSpec (spec) where

import Calc
import Control.Monad (void)
import Data.Functor (($>))
import Data.Text (Text)
import Test.Hspec
import qualified Data.List.NonEmpty as NE
import Data.Bifunctor (bimap)
import Calc.Patterns.Flatten (generateMissing)
import Test.Helpers

-- | try parsing the input, exploding if it's invalid
unsafeParsePattern :: Text -> (Expr (), NE.NonEmpty (Pattern (), Expr ()))
unsafeParsePattern input = case parseExprAndFormatError input of
  Right (EPatternMatch _ expr pats) ->
      (expr $> (), fmap (bimap void void) pats )
  Right other -> error $ "expected pattern match, got " <> show other
  Left e -> error (show e)

spec :: Spec
spec = do
  fdescribe "PatternsSpec" $ do
    it "Wildcard is exhaustive" $ do
      let (_expr,pats) = unsafeParsePattern "case a of _ -> 2"
      generateMissing (fst <$> pats) `shouldBe` []
    it "True needs false" $ do
      let (_expr,pats) = unsafeParsePattern "case a of True -> 2"
      generateMissing (fst <$> pats) `shouldBe` [patBool False ]
    it "False needs True" $ do
      let (_expr,pats) = unsafeParsePattern "case a of False -> 2"
      generateMissing (fst <$> pats) `shouldBe` [patBool True]
    it "False and True needs nothing" $ do
      let (_expr,pats) = unsafeParsePattern "case a of False -> 2 | True -> 1"
      generateMissing (fst <$> pats) `shouldBe` []
    it "Tuple of two wildcards needs nothing " $ do
      let (_expr,pats) = unsafeParsePattern "case a of (_,_) -> True"
      generateMissing (fst <$> pats) `shouldBe` []
    it "Tuple of one wildcard and one true needs a false" $ do
      let (_expr,pats) = unsafeParsePattern "case a of (_,True) -> True"
      generateMissing (fst <$> pats) `shouldBe` [patTuple [PWildcard (), patBool False]]
    it "Tuple of one true and one false needs a bunch" $ do
      let (_expr,pats) = unsafeParsePattern "case a of (False,True) -> True"
      generateMissing (fst <$> pats) `shouldBe` [patTuple [patBool False, patBool False],
                                                  patTuple [patBool True, patBool True],
                                                  patTuple [patBool True, patBool False]]
    it "Tuple of booleans with some things supplied" $ do
      let (_expr,pats) = unsafeParsePattern "case a of (False,True) -> True | (True, False) -> False"
      generateMissing (fst <$> pats) `shouldBe` [patTuple [patBool False, patBool False],
                                                  patTuple [patBool True, patBool True]]

    it "Tuple of booleans with some things supplied" $ do
      let (_expr,pats) = unsafeParsePattern "case a of (False,True) -> True | (True, False) -> False"
      generateMissing (fst <$> pats) `shouldBe` [patTuple [patBool False, patBool False],
                                                  patTuple [patBool True, patBool True]]

    it "Tuple of wildcard and boolean" $ do
      let (_expr,pats) = unsafeParsePattern "case a of (False,_) -> True | (True, False) -> False"
      generateMissing (fst <$> pats) `shouldBe` [
                                                  patTuple [patBool True, patBool True]]







