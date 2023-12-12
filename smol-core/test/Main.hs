module Main (main) where

import Test.Hspec
import qualified Test.ParserSpec
import qualified Test.TransformSpec

main :: IO ()
main = hspec $ parallel $ do
  Test.ParserSpec.spec
  Test.TransformSpec.spec
