{-# LANGUAGE OverloadedStrings #-}

module Test.Patterns.PatternsSpec (spec) where

import Calc
import Control.Monad (void)
import Data.Functor (($>))
import Data.Text (Text)
import Test.Hspec
import qualified Data.List.NonEmpty as NE
import Data.Bifunctor (bimap)
import Calc.Patterns.Flatten (flattenPatterns, SimpleExpr(..),SimplePattern(..))

-- | try parsing the input, exploding if it's invalid
unsafeParsePattern :: Text -> (Expr (), NE.NonEmpty (Pattern (), Expr ()))
unsafeParsePattern input = case parseExprAndFormatError input of
  Right (EPatternMatch _ expr pats) ->
      (expr $> (), fmap (bimap void void) pats )
  Right other -> error $ "expected pattern match, got " <> show other
  Left e -> error (show e)

spec :: Spec
spec = do
  describe "PatternsSpec" $ do
    it "Trivial wildcard pattern converts without trouble" $ do
      let (_expr,pats) = unsafeParsePattern "case a of _ -> 2"
      flattenPatterns pats `shouldBe` [(SPWildcard, SEPrim (PInt 2))]
    it "Trivial primitive pattern converts without trouble" $ do
      let (_expr,pats) = unsafeParsePattern "case a of 1 -> 2 | _ -> 0"
      flattenPatterns pats `shouldBe` [(SPPrim (PInt 1), SEPrim (PInt 2)),
                (SPWildcard, SEPrim (PInt 0))]
    it "Tuple is split into two patterns" $ do
      let (_expr,pats) = unsafeParsePattern "case p of (1,2) -> True | (_,_) -> False"
      flattenPatterns pats `shouldBe` [(SPPrim (PInt 1), SEPrim (PInt 2)),
                (SPWildcard, SEPrim (PInt 0))]

