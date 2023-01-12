{-# LANGUAGE OverloadedStrings #-}

module CoreTest.Source.SourceSpan (spec) where

import Language.Mimsa.Core
import Test.Hspec

spec :: Spec
spec =
  describe "SourceSpan" $ do
    it "Simple" $
      sourceSpan "dog" (Location 0 2)
        `shouldBe` Just
          ( SourceSpan
              { ssRowStart = 1,
                ssRowEnd = 1,
                ssColStart = 1,
                ssColEnd = 3
              }
          )
    it "Second line" $
      sourceSpan "dog\nlog" (Location 4 6)
        `shouldBe` Just
          ( SourceSpan
              { ssRowStart = 2,
                ssRowEnd = 2,
                ssColStart = 1,
                ssColEnd = 3
              }
          )
    it "Across lines" $
      sourceSpan "a\ngood\ndog" (Location 2 9)
        `shouldBe` Just
          ( SourceSpan
              { ssRowStart = 2,
                ssRowEnd = 3,
                ssColStart = 1,
                ssColEnd = 3
              }
          )
