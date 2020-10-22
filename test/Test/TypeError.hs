{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.TypeError
  ( spec,
  )
where

import qualified Data.List.NonEmpty   as NE
import qualified Data.Set             as S
import           Data.Text            (Text)
import           Language.Mimsa.Types
import           Test.Hspec
import           Text.Megaparsec

showError :: Text -> Annotation -> String
showError input ann = let errorBundle = createErrorBundle input ann in
                          errorBundlePretty errorBundle

instance ShowErrorComponent Annotation where
  showErrorComponent None           = "Some sort of error"
  showErrorComponent (Location _ _) = "Is this some sort of good boy?"
  errorComponentLen None           = 0
  errorComponentLen (Location a b) = b - a

toFancy :: Annotation -> ParseError Text Annotation
toFancy None = FancyError 0 (S.singleton (ErrorCustom None))
toFancy (Location a b) = FancyError a (S.singleton (ErrorCustom $ Location a b))

createErrorBundle :: Text -> Annotation -> ParseErrorBundle Text Annotation
createErrorBundle input ann =
    let initialState =
            PosState
              { pstateInput = input
              , pstateOffset = 0
              , pstateSourcePos = initialPos "repl"
              , pstateTabWidth = defaultTabWidth
              , pstateLinePrefix = ""
              }
            in ParseErrorBundle
              { bundleErrors = NE.fromList [toFancy ann]
                            -- ^ A collection of 'ParseError's that is sorted by parse error offsets
              , bundlePosState = initialState
                            -- ^ State that is used for line\/column calculation
              }




spec :: Spec
spec = do
  describe "TypeError" $ do
    it "Displays the location of an error" $ do
      let input = "if dog then 1 else 2"
          location = Location 4 7
      let result = showError input location
      result `shouldBe` ""
