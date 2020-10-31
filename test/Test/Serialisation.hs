{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Serialisation
  ( spec,
  )
where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding
import Language.Mimsa.Parser (parseExprAndFormatError)
import Language.Mimsa.Printer
import Language.Mimsa.Types
import Test.Hspec
import Test.Utils.Serialisation

type StoreExpr = StoreExpression ()

parseExprFromPretty :: String -> IO (Maybe Text)
parseExprFromPretty filename =
  loadRegression
    filename
    (hush . prettyPrintingParses . toStrict . decodeUtf8)

hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _ = Nothing

-- remove annotations for comparison
toEmptyAnn :: Expr a b -> Expr a ()
toEmptyAnn = toEmptyAnnotation

-- does the output of our prettyprinting still make sense to the parser?
prettyPrintingParses :: Text -> Either Text Text
prettyPrintingParses input = do
  expr1 <- parseExprAndFormatError input
  case parseExprAndFormatError (prettyPrint expr1) of
    Left e -> Left e
    Right expr2 ->
      if toEmptyAnn expr1 /= toEmptyAnn expr2
        then Left "Does not match"
        else pure input

spec :: Spec
spec =
  describe "Serialisation" $ do
    it "JSON" $ do
      files <- getAllFilesInDir "StoreExpression"
      loaded <- traverse (loadJSON @StoreExpr) files
      length loaded `shouldBe` length (catMaybes loaded)
    it "Pretty printing" $ do
      files <- getAllFilesInDir "PrettyPrinting"
      loaded <- traverse parseExprFromPretty files
      length loaded `shouldBe` length (catMaybes loaded)
