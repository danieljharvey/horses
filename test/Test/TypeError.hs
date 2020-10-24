{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.TypeError
  ( spec,
  )
where

import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.DisplayError
import Language.Mimsa.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "TypeError" $ do
    it "Contains the error message" $ do
      let input = "if dog then 1 else 2"
          err = UnknownTypeError
      let result = displayError input err
      T.putStrLn result
      result `shouldSatisfy` (\a -> T.unpack (prettyPrint err) `isInfixOf` T.unpack a)
    it "Shows the location with CannotCaseMatchOnType" $ do
      let input = "case blah of Just \a -> a | Nothing False"
          err = CannotCaseMatchOnType (MyVar (Location 5 9) (NamedVar $ mkName "blah"))
          result = displayError input err
      T.putStrLn result
      result `shouldSatisfy` (\a -> "^^^^" `isInfixOf` T.unpack a)
