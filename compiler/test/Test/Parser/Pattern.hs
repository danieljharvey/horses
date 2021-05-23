{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.Pattern
  ( spec,
  )
where

import Data.Functor
import Data.Text (Text)
import Language.Mimsa.Parser
import Language.Mimsa.Parser.Pattern
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
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

spec :: Spec
spec =
  describe "Pattern" $ do
    it "Parses constructor with no args" $
      testParse "None" `shouldBe` Right (PConstructor mempty "None" mempty)
    it "Parses constructor with 1 arg" $
      testParse "Some _" `shouldBe` Right (PConstructor mempty "Some" [PWildcard mempty])
    it "Parses constructor with 2 args" $
      testParse "Some _ 1"
        `shouldBe` Right
          ( PConstructor
              mempty
              "Some"
              [ PWildcard mempty,
                PLit mempty (MyInt 1)
              ]
          )
