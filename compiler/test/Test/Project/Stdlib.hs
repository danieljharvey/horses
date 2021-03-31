{-# LANGUAGE OverloadedStrings #-}

module Test.Project.Stdlib
  ( spec,
  )
where

import Data.Either
import Language.Mimsa.Project.Stdlib
import Test.Hspec

spec :: Spec
spec =
  describe "Stdlib" $ do
    it "Builds" $
      buildStdlib `shouldSatisfy` isRight
