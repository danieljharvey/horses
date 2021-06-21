{-# LANGUAGE OverloadedStrings #-}

module Test.Codegen.Enum
  ( spec,
  )
where

import Data.Either (isRight)
import Language.Mimsa.Codegen
import Test.Codegen.Shared
import Test.Hspec

spec :: Spec
spec = do
  describe "Enum instances" $ do
    it "Enum dtTrafficLights typechecks" $ do
      typecheckInstance toString dtTrafficLights `shouldSatisfy` isRight

    it "Generates toString for dtTrafficLights" $ do
      let expected =
            unsafeParse
              "\\trafficLights -> match trafficLights with Green -> \"Green\" | Red -> \"Red\" | Yellow -> \"Yellow\""

      toString dtTrafficLights
        `shouldBe` Right expected
