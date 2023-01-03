{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.Pattern
  ( spec,
  )
where

import Language.Mimsa.Core
import Data.Functor
import Data.List (isInfixOf)
import Data.Text (Text)
import Test.Hspec
import Text.Megaparsec

-- parse pat, using it all up
parsePat :: Text -> Either ParseErrorType ParserPattern
parsePat = parse (patternParser <* eof) "repl"

-- specialisation of parseExpr
testParse :: Text -> Either String (Pattern Name ())
testParse t = case parsePat t of
  Right expr -> pure (expr $> ())
  Left e -> Left $ errorBundlePretty e

errorContains :: String -> Either String a -> Bool
errorContains s res = case res of
  Left e -> s `isInfixOf` e
  _ -> False

spec :: Spec
spec =
  describe "Pattern" $ do
    it "Parses constructor with no args" $
      testParse "None" `shouldBe` Right (PConstructor mempty Nothing "None" mempty)
    it "Parses constructor with 1 arg" $
      testParse "Some _" `shouldBe` Right (PConstructor mempty Nothing "Some" [PWildcard mempty])
    it "Parses constructor with 2 args" $
      testParse "Some _ 1"
        `shouldBe` Right
          ( PConstructor
              mempty
              Nothing
              "Some"
              [ PWildcard mempty,
                PLit mempty (MyInt 1)
              ]
          )
    it "Parses array with two items" $
      testParse "[1,a]"
        `shouldBe` Right
          ( PArray
              mempty
              [ PLit mempty (MyInt 1),
                PVar mempty "a"
              ]
              NoSpread
          )
    it "Parses array with wildcard spread" $
      testParse "[1, a, ...]"
        `shouldBe` Right
          ( PArray
              mempty
              [ PLit mempty (MyInt 1),
                PVar mempty "a"
              ]
              ( SpreadWildcard
                  mempty
              )
          )
    it "Parses array with value spread" $
      testParse "[1, a, ...b]"
        `shouldBe` Right
          ( PArray
              mempty
              [ PLit mempty (MyInt 1),
                PVar mempty "a"
              ]
              ( SpreadValue
                  mempty
                  "b"
              )
          )
    it "Spread needs at least one value" $
      testParse "[...b]"
        `shouldSatisfy` errorContains "There must be at least one pattern"

    it "Cannot have more than one spread" $
      testParse "[1, ...a,...b]"
        `shouldSatisfy` errorContains "Cannot have more than one spread"
    it "Trailing comma in pattern" $
      testParse "[1,2,]"
        `shouldSatisfy` errorContains "Expected pattern or a spread operator"
    it "Parses pattern for non-empty string" $
      testParse "_ ++ _"
        `shouldBe` Right
          ( PString
              mempty
              (StrWildcard mempty)
              (StrWildcard mempty)
          )
