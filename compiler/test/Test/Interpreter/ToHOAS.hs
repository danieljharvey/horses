{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Test.Interpreter.ToHOAS
  ( spec,
  )
where

import Language.Mimsa.Interpreter.ToHOAS
import Test.Hspec
import Test.Utils.Helpers
import Data.Text
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Data.Bifunctor

parseExpr :: Text -> Expr (Name,()) ()
parseExpr input = first (,()) (unsafeParseExpr input)

spec :: Spec
spec = do
  describe "ToHOAS" $ do
    describe "There and back again" $ do
      it "Infixes, literals" $ do
        let input = parseExpr "1 + 2 + 3"
            result = fromHOAS (toHOAS input)
        result `shouldBe` input
      it "uses module" $ do
        let input = parseExpr "Prelude.fst (1,2)"
            result = fromHOAS (toHOAS input)
        result `shouldBe` input
      it "A function appears" $ do
        let input = parseExpr "\\a -> a"
            result = fromHOAS (toHOAS input)
        result `shouldBe` input
      it "A let" $ do
        let input = parseExpr "let a = 1 in a"
            result = fromHOAS (toHOAS input)
        result `shouldBe` input
      it "Array spread pattern match" $ do
        let input = parseExpr "\\as -> match as with [a,b,...c] -> (a + b, c) | _ -> (0,[])"
            result = fromHOAS (toHOAS input)
        result `shouldBe` input
      it "String spread pattern match" $ do
        let input = parseExpr "\\as -> match as with a ++ tail -> tail | _ -> \"1\""
            result = fromHOAS (toHOAS input)
        result `shouldBe` input
      it "Recursive function" $ do
        let input = parseExpr "let again = \\a -> if a > 0 then again (a - 1) else 0; True"
            result = fromHOAS (toHOAS input)
        result `shouldBe` input
      it "A pattern match appears" $ do
        let input = parseExpr "match (1,2) with (a,b) -> a"
            result = fromHOAS (toHOAS input)
        result `shouldBe` input
