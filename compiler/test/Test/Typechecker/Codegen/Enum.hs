{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Codegen.Enum
  ( spec,
  )
where

import Data.Either (isRight)
import Language.Mimsa.Typechecker.Codegen
import Test.Hspec
import Test.Typechecker.Codegen.Shared

spec :: Spec
spec = do
  describe "Enum instances" $ do
    it "Generates toString for dtTrafficLights" $ do
      typecheckInstance toString dtTrafficLights `shouldSatisfy` isRight
      let expected =
            unsafeParse
              "\\trafficLights -> case trafficLights of Green \"Green\" | Red \"Red\" | Yellow \"Yellow\""

      toString dtTrafficLights
        `shouldBe` Right expected
