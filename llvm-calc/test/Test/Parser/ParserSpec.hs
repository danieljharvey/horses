{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.ParserSpec (spec) where

import Data.Bifunctor (second)
import Data.Either (isRight)
import Data.FileEmbed
import Data.Foldable (traverse_)
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Calc
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = do
    describe "Expr" $ do
      let strings =
            [ ("True", bool True),
              ("False", bool False),
              ("-1", int (-1)),
              ("1 + 2", EInfix () OpAdd (nat 1) (nat 2)),
              ("1 + 2 + 3", EInfix () OpAdd (EInfix () OpAdd (nat 1) (nat 2)) (nat 3))
            ]
      traverse_
        ( \(str, expr) -> it (T.unpack str) $ do
            case parseExprAndFormatError str of
              Right parsedExp -> parsedExp $> () `shouldBe` expr
              Left e -> error (T.unpack e)
        )
        strings
