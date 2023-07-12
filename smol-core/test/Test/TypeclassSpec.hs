{-# LANGUAGE OverloadedStrings #-}

module Test.TypeclassSpec (spec) where

import Control.Monad.Identity
import Data.Either (isRight)
import Smol.Core.Types.Identifier
import Smol.Core.Types.Type
import Smol.Core.Types.Typeclass
import Test.Helpers
import Test.Hspec

tcVar :: (Monoid ann) => Identifier -> Type Identity ann
tcVar i = TVar mempty (Identity i)

-- | tests for checking modules make sense
spec :: Spec
spec = do
  describe "Typeclasses" $ do
    describe "addTypeclass" $ do
      it "Adds the first typeclass with no superclasses OK" $ do
        addTypeclass "Eq" [] mempty `shouldSatisfy` isRight

      it "Adding Eq twice is an error" $ do
        (addTypeclass "Eq" [] mempty >>= addTypeclass "Eq" [])
          `shouldBe` Left (AlreadyDefined "Eq")

      it "Adding Ord without Eq is an error" $ do
        addTypeclass "Ord" ["Eq"] mempty
          `shouldBe` Left (SuperclassNotFound "Eq")

      it "Adding Eq and then Ord is OK" $ do
        (addTypeclass "Eq" [] mempty >>= addTypeclass "Ord" ["Eq"])
          `shouldSatisfy` isRight

    describe "addInstance" $ do
      it "Adds instance for unknown typeclass" $ do
        addInstance [] (IsIn "Horse" tyUnit) mempty
          `shouldBe` Left (NoClassFoundForInstance "Horse")

      it "Adds Eq instances for Unit" $ do
        (builtInTypeclasses >>= addInstance [] (IsIn "Eq" tyUnit))
          `shouldSatisfy` isRight

      it "Adds Eq instances for Unit, Int and pair" $ do
        ( builtInTypeclasses
            >>= addInstance [] (IsIn "Eq" tyUnit)
            >>= addInstance [] (IsIn "Eq" tyInt)
            >>= addInstance
              [ IsIn "Eq" (tcVar "a"),
                IsIn "Eq" (tcVar "b")
              ]
              (IsIn "Eq" (tyTuple (tcVar "a") [tcVar "b"]))
          )
          `shouldSatisfy` isRight

    describe "bySuper" $ do
      it "Eq Int has no superclasses" $ do
        let typeclasses = builtInTypeclasses >>= 
                 addInstance [] (IsIn "Eq" tyUnit)
        
        flip bySuper (IsIn "Eq" tyUnit) <$> typeclasses `shouldBe` Right [IsIn "Eq" tyUnit]

      it "Ord Int has one superclass" $ do
        let typeclasses = builtInTypeclasses >>= 
                 addInstance [] (IsIn "Eq" tyUnit)
                 >>= addInstance [] (IsIn "Ord" tyUnit)
        
        flip bySuper (IsIn "Ord" tyUnit) <$> typeclasses `shouldBe` 
            Right [IsIn "Ord" tyUnit, IsIn "Eq" tyUnit]
