{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.TransformSpec (spec) where

import Control.Monad (void)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Smol.Core.Parser
import Smol.Core.Transform.BetaReduce
import Smol.Core.Transform.EtaReduce
import Smol.Core.Transform.FlattenLets
import Smol.Core.Transform.FloatDown
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = do
  describe "BetaReduce" $ do
    let singleDefs =
          [ ("id", "id"),
            ("(\\a -> a + 1) b", "let a = b in a + 1"),
            ("(\\a -> \\b -> a + b) a1 b2", "let a = a1; let b = b2; a + b"),
            ("(\\a -> a + 1 : Int -> Int) b", "let a = b in a + 1"),
            ("if True then 1 else 2", "1"),
            ("if False then 1 else 2", "2"),
            ("{ a: 1, b: 2 }.a", "1")
          ]

    traverse_
      ( \(input, expectText) -> it ("Beta reduces: " <> input) $ do
          let expr = getRight $ parseExprAndFormatError (T.pack input)
              expected = getRight $ parseExprAndFormatError (T.pack expectText)

          void (betaReduce expr) `shouldBe` void expected
      )
      singleDefs

  describe "EtaReduce" $ do
    let singleDefs =
          [ ("id", "id"),
            ("\\a -> id a", "id"),
            ("\\a -> \\b -> id a b", "id"),
            ("\\a -> \\b -> \\c -> id a b c", "id")
          ]

    traverse_
      ( \(input, expectText) -> it ("Eta reduces: " <> input) $ do
          let expr = getRight $ parseExprAndFormatError (T.pack input)
              expected = getRight $ parseExprAndFormatError (T.pack expectText)

          void (etaReduce expr) `shouldBe` void expected
      )
      singleDefs

  describe "FlattenLets" $ do
    let singleDefs =
          [ ("id", "id"),
            ( "let a = (let b = 1 in b + 1) in a + 1",
              "let b = 1; let a = b + 1; a + 1"
            ),
            ( "let a = (let b = (let c = 1 in c + 1) in b + 1) in a + 1",
              "let c = 1; let b = c + 1; let a = b + 1; a + 1"
            )
          ]

    traverse_
      ( \(input, expectText) -> it ("Flattens lets: " <> input) $ do
          let expr = getRight $ parseExprAndFormatError (T.pack input)
              expected = getRight $ parseExprAndFormatError (T.pack expectText)

          void (flattenLets expr) `shouldBe` void expected
      )
      singleDefs

  describe "FloatDown" $ do
    let singleDefs =
          [ ("id", "id"),
            ( "let a = 1; case a of True -> 1 | False -> 2",
              "let a = 1; case a of True -> 1 | False -> 2"
            ),
            ( "let a = 1; case b of True -> 1 | False -> 2",
              "case b of True -> let a = 1; 1 | False -> let a = 1; 2"
            ),
            ("let a = 1; if a then 1 else 2", "let a = 1; if a then 1 else 2"),
            ( "let a = 1; if b then 1 else 2",
              "if b then let a = 1; 1 else let a = 1; 2"
            ),
            ("let a = 1; let b = 2; if c then 1 else 2",
              "if c then let a = 1; let b = 2; 1 else let a = 1; let b = 2; 2"
            )
          ]

    traverse_
      ( \(input, expectText) -> it ("Flattens lets: " <> input) $ do
          let expr = getRight $ parseExprAndFormatError (T.pack input)
              expected = getRight $ parseExprAndFormatError (T.pack expectText)

          void (floatDown expr) `shouldBe` void expected
      )
      singleDefs
