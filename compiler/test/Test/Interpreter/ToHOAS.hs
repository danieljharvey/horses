{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Test.Interpreter.ToHOAS
  ( spec,
  )
where

import Data.Bifunctor
import Data.Text
import Language.Mimsa.Core hiding (parseExpr)
import qualified Language.Mimsa.Interpreter.HOASExpr as HOAS
import Language.Mimsa.Interpreter.ToHOAS
import Test.Hspec
import Test.Utils.Helpers

parseExpr :: Text -> Expr (Name, ()) ()
parseExpr input = first (,()) (unsafeParseExpr input)

spec :: Spec
spec = do
  describe "ToHOAS" $ do
    describe "Are we creating recursive lambdas when we should?" $ do
      it "Single arg recursive function" $ do
        let input = parseExpr "let loop = \\a -> loop (a - 1) in loop 0"
            result = toHOAS input
        result `shouldSatisfy` \case
          (HOAS.MyApp _ HOAS.MyRecursiveLambda {} HOAS.MyLambda {}) -> True
          _ -> False
      it "Double arg recursive function" $ do
        let input = parseExpr "let loop = \\a -> \\b -> loop (a - 1) True in loop 0 False"
            result = toHOAS input
        result `shouldSatisfy` \case
          (HOAS.MyApp _ HOAS.MyRecursiveLambda {} HOAS.MyLambda {}) -> True
          _ -> False

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
      it "var with module" $ do
        let input = parseExpr "let a = Prelude.id 1 in a"
            result = fromHOAS (toHOAS input)
        result `shouldBe` input
      it "A let pattern" $ do
        let input = parseExpr "let (a,b,c) = (1,2,3) in a + b + c"
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
        let input = parseExpr "let loop = \\a -> if a > 0 then loop (a - 1) else 0; let b = loop 100 in b"
            result = fromHOAS (toHOAS input)
        result `shouldBe` input
      it "A pattern match appears" $ do
        let input = parseExpr "match (1,2) with (a,b) -> a"
            result = fromHOAS (toHOAS input)
        result `shouldBe` input
      it "A pattern match with multiple bindings appears" $ do
        let input = parseExpr "match (1,2) with (a,b) -> a + b"
            result = fromHOAS (toHOAS input)
        result `shouldBe` input
      it "A pattern match with multiple bindings appears" $ do
        let input = parseExpr "match (1,2) with (a,b) -> Prelude.id (a + b)"
            result = fromHOAS (toHOAS input)
        result `shouldBe` input

      it "A big filter function" $ do
        let input = parseExpr "let filter = \\pred -> \\str -> let fn = (\\s -> match s with a ++ as -> let rest = fn as; if pred a then a ++ rest else rest | _ -> \"\") in fn str; filter (\\aa -> aa == \"o\") \"woo\""
            result = fromHOAS (toHOAS input)
        result `shouldBe` input

      it "Parser.many" $ do
        let input = parseExpr "\\parser -> let (Parser innerP) = parser; (Parser (\\input -> let go items i = match (innerP i) with (Maybe.Just (a, i2)) -> (go (items <> [ a ]) i2) | (Maybe.Nothing) -> (Maybe.Just ((items, i))); go [] input))"
            result = fromHOAS (toHOAS input)
        result `shouldBe` input
