module Main
  ( main,
  )
where

import qualified Test.Builder.BuilderSpec
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    Test.Builder.BuilderSpec.spec
