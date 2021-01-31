{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Codegen
  ( spec,
  )
where

import qualified Data.Map as M
import Language.Mimsa.Typechecker.Codegen
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers ()
import Test.Hspec

dtVoid :: DataType
dtVoid = DataType "Void" mempty mempty

dtTrafficLights :: DataType
dtTrafficLights =
  DataType
    "TrafficLights"
    mempty
    ( M.fromList
        [ ("Red", mempty),
          ("Yellow", mempty),
          ("green", mempty)
        ]
    )

spec :: Spec
spec = do
  describe "Codegen" $ do
    it "No instances for Void" $ do
      typeclassMatches dtVoid `shouldBe` mempty
    it "Show instance for TrafficLights" $ do
      typeclassMatches dtTrafficLights `shouldSatisfy` elem Show
