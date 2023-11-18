{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Modules.ParserSpec (spec) where

import Data.Bifunctor (second)
import Data.Either (isRight)
import Data.FileEmbed
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Smol.Core.Modules.Parser
import Test.Hspec

-- these are saved in a file that is included in compilation
testInputs :: [(FilePath, Text)]
testInputs =
  fmap (second T.decodeUtf8) $(makeRelativeToProject "test/static/" >>= embedDir)

spec :: Spec
spec = do
  describe "Parser" $ do
    describe "Module" $ do
      let singleDefs =
            [ "type Dog a = Woof String | Other a",
              "def id : a -> a",
              "def id a = a",
              "def compose f g a = f (g a)",
              "def compose : (c -> b) -> (a -> b) -> (a -> c)",
              "def onePlusOneEqualsTwo = 1 + 1 == 2",
              "test \"one plus one equals two\" { 1 + 1 == 2 }",
              "def usesEquals : (Eq (a,b)) => (a,b) -> (a,b) -> Bool",
              "class Eq a { equals: a -> a -> Bool }",
              "instance Eq Int { eqInt }",
              "instance (Eq a) => Eq (Maybe a) { eqMaybeA }"
            ]

      it "All defs" $ do
        let result = parseModuleAndFormatError (T.intercalate "\n" (T.pack <$> singleDefs))
        result `shouldSatisfy` isRight

      traverse_
        ( \input -> it ("Parses module item: " <> input) $ do
            let result = parseModuleAndFormatError (T.pack input)

            result `shouldSatisfy` isRight
        )
        singleDefs

      traverse_
        ( \(filename, contents) ->
            it ("Parses " <> filename) $ do
              let result = parseModuleAndFormatError contents
              result `shouldSatisfy` isRight
        )
        testInputs
