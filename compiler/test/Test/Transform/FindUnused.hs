{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Transform.FindUnused
  ( spec,
  )
where

import qualified Data.Set as S
import Language.Mimsa.Transform.FindUnused
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "FindUnused" $ do
    it "Nothing in literal" $ do
      findUnused @Name @Annotation (bool True)
        `shouldBe` mempty
    it "Finds X in simple Let assignment" $ do
      findUnused @Name @Annotation
        (MyLet mempty (Identifier mempty "a") (bool True) (bool True))
        `shouldBe` S.singleton "a"
    it "Does not find X when it is returned later from Let" $ do
      findUnused @Name @Annotation
        (MyLet mempty (Identifier mempty "a") (bool True) (MyVar mempty "a"))
        `shouldBe` mempty
