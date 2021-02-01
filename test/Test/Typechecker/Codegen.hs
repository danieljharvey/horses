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

-- | has no constructors, we can do nothing with this
dtVoid :: DataType
dtVoid = DataType "Void" mempty mempty

-- | an enum, we can go to and from a string
dtTrafficLights :: DataType
dtTrafficLights =
  DataType
    "TrafficLights"
    mempty
    ( M.fromList
        [ ("Red", mempty),
          ("Yellow", mempty),
          ("Green", mempty)
        ]
    )

-- | A newtype around a string
-- | we can wrap and unwrap maybe?
dtWrappedString :: DataType
dtWrappedString =
  DataType "WrappedString" mempty (M.singleton "Wrapped" [VarName "String"])

spec :: Spec
spec = do
  describe "Codegen" $ do
    it "No instances for Void" $ do
      typeclassMatches dtVoid `shouldBe` mempty
    it "Enum instance for TrafficLights" $ do
      typeclassMatches dtTrafficLights `shouldSatisfy` elem Enum
    it "No enum instance for WrappedString" $ do
      typeclassMatches dtWrappedString `shouldBe` mempty
