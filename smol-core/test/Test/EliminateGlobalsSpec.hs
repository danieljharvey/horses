{-# LANGUAGE OverloadedStrings #-}

module Test.EliminateGlobalsSpec (spec) where

import Data.Foldable (traverse_)
import Data.Functor
import qualified Data.Text as T
import Smol.Core
import Smol.Core.EliminateGlobals
import Test.Hspec

spec :: Spec
spec = do
  describe "Eliminate Globals" $ do
    fdescribe "Expr" $ do
      let strings =
            [ ("True", "True"),
              ("False", "False"),
              ("global egg = 1; egg!", "let egg = 1; egg"), -- if we resolve the global within our expression, it's just a spicy let
              ("dog!", "\\innerargs -> innerargs.dog"), -- globals are lifted to lambdas
              ("\\a -> dog!", "\\innerargs -> \\a -> innerargs.dog"), -- those lambdas go outside existing lambdas
              ("\\what -> (log! 1, dog! 2)","\\innerargs -> \\what -> (innerargs.log 1, innerargs.dog 2)")
            ]
      traverse_
        ( \(str, expectedStr) -> it (T.unpack str) $ do
            case (,) <$> parseExprAndFormatError str <*> parseExprAndFormatError expectedStr of
              Right (parsedExp, expected) -> eliminateGlobals emptyParseDep (parsedExp $> ()) `shouldBe` (expected $> ())
              Left e -> error (T.unpack e)
        )
        strings
