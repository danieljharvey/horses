{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.TypeError
  ( spec,
  )
where

import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.DisplayError
import Language.Mimsa.Types.Error
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

textContains :: Text -> Text -> Bool
textContains needle haystack =
  T.unpack needle `isInfixOf` T.unpack haystack

getTypeError :: Text -> Maybe Text
getTypeError input =
  case evaluateText testStdlib input of
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
      result `shouldSatisfy` textContains (prettyPrint (err :: TypeError))
    it "Shows the location with CannotMatchRecord" $
      getTypeError "let dog = True in dog.tail"
        `shouldSatisfy` maybePred (textContains "^^^^^^^^")
    it "Shows the location with UnificationError" $
      getTypeError "if 100 then 1 else 2"
        `shouldSatisfy` maybePred (textContains "^^^")
    it "Shows the location with MissingRecordMember" $
      getTypeError "let a = {} in a.dog"
        `shouldSatisfy` maybePred (textContains "^^^^^")
