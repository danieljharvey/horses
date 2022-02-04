{-# LANGUAGE OverloadedStrings #-}

module Test.Project.NormaliseType
  ( spec,
  )
where

import Data.Coerce
import Data.Text (Text)
import Language.Mimsa.Typechecker.NormaliseTypes
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Hspec

mkVar :: Int -> MonoType
mkVar i = MTVar mempty (TVUnificationVar i)

mkNameVar :: Text -> MonoType
mkNameVar n = MTVar mempty (TVName Nothing (coerce n))

normaliseType' :: MonoType -> MonoType
normaliseType' = normaliseType

spec :: Spec
spec =
  describe "Normalise type" $ do
    it "Literals are the same" $
      normaliseType' (MTPrim mempty MTInt)
        `shouldBe` MTPrim mempty MTInt
    it "Unification var starts at 1" $
      normaliseType' (mkVar 10)
        `shouldBe` mkVar 1
    it "The same vars should get the same numbers " $
      normaliseType' (MTPair mempty (mkVar 10) (mkVar 10))
        `shouldBe` MTPair mempty (mkVar 1) (mkVar 1)
    it "We increase the value we return as we go" $
      normaliseType' (MTPair mempty (mkVar 10) (mkVar 8))
        `shouldBe` MTPair mempty (mkVar 1) (mkVar 2)
    it "Repeating an earlier value does not break it" $
      normaliseType'
        ( MTPair
            mempty
            (mkVar 10)
            (MTPair mempty (mkVar 8) (mkVar 10))
        )
        `shouldBe` MTPair
          mempty
          (mkVar 1)
          ( MTPair
              mempty
              (mkVar 2)
              (mkVar 1)
          )
    it "Normalises named variables too" $
      normaliseType'
        ( MTFunction
            mempty
            (mkNameVar "a")
            (MTFunction mempty (mkNameVar "b") (mkNameVar "a"))
        )
        `shouldBe` MTFunction
          mempty
          (mkVar 1)
          ( MTFunction
              mempty
              (mkVar 2)
              (mkVar 1)
          )
