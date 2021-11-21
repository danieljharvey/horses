{-# LANGUAGE OverloadedStrings #-}

module Test.Project.SourceSpan (spec) where

import Language.Mimsa.Project.SourceSpan
import Language.Mimsa.Types.AST.Annotation
import Language.Mimsa.Types.Project.SourceSpan
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
