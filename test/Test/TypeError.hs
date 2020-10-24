{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.TypeError
  ( spec,
  )
where

import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Actions
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.DisplayError
import Language.Mimsa.Types
import Test.Hspec

textContains :: Text -> Text -> Bool
textContains needle haystack =
  T.unpack needle `isInfixOf` T.unpack haystack

getTypeError :: Text -> Maybe Text
getTypeError input =
  case evaluateText mempty input of
    Left e -> Just (prettyPrint e)
    _ -> Nothing

maybePred :: (a -> Bool) -> Maybe a -> Bool
maybePred _ Nothing = False
maybePred pred' (Just a) = pred' a

spec :: Spec
spec = do
  describe "TypeError" $ do
    it "Contains the error message" $ do
      let input = "if dog then 1 else 2"
          err = UnknownTypeError
      let result = displayError input err
      result `shouldSatisfy` textContains (prettyPrint err)
    it "Shows the location with CannotCaseMatchOnType" $
      getTypeError "case blah of Just \a -> a | Nothing False"
        `shouldSatisfy` maybePred (textContains "^^^^")
    it "Shows the location with CaseMatchExpectedPair" $
      getTypeError "let (a,b) = True in a"
        `shouldSatisfy` maybePred (textContains "^^^^")
