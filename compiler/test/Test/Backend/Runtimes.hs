{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.Runtimes
  ( spec,
  )
where

import Data.Either (isLeft, isRight)
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Types.Identifiers ()
import Language.Mimsa.Types.Typechecker
import Test.Hspec

spec :: Spec
spec = do
  describe "Runtimes" $ do
    it "String is allowed by console runtime" $ do
      let mt = MTPrim mempty MTString
      let result = runtimeIsValid cjsConsoleRuntime mt
      result `shouldSatisfy` isRight
    it "Int is not allowed by console runtime" $ do
      let mt = MTPrim mempty MTInt
      let result = runtimeIsValid cjsConsoleRuntime mt
      result `shouldSatisfy` isLeft
    it "Int is allowed by module export runtime" $ do
      let mt = MTPrim mempty MTInt
      let result = runtimeIsValid cjsExportRuntime mt
      result `shouldSatisfy` isRight
