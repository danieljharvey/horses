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
  describe "FindUnused" $
    do
      it "Nothing in literal" $ do
        findUnused @Name @Annotation (bool True)
          `shouldBe` mempty
      it "Finds `a` in simple Let assignment" $ do
        findUnused @Name @Annotation
          (MyLet mempty (Identifier mempty "a") (bool True) (bool True))
          `shouldBe` S.singleton ("a", mempty)
      it "Does not find `a` when it is returned later from Let" $ do
        findUnused @Name @Annotation
          (MyLet mempty (Identifier mempty "a") (bool True) (MyVar mempty "a"))
          `shouldBe` mempty
      it "Finds `a` in a pattern match" $ do
        findUnused @Name @Annotation
          (MyPatternMatch mempty (bool True) [(PVar mempty "a", bool True)])
          `shouldBe` S.singleton ("a", mempty)
      it "Does not find `a` when it is used in a pattern match" $ do
        findUnused @Name @Annotation
          (MyPatternMatch mempty (bool True) [(PVar mempty "a", MyVar mempty "a")])
          `shouldBe` mempty

  describe "removeUnused" $ do
    it "No change in literal" $ do
      let expr = bool True
      removeUnused @Name @Annotation (S.singleton "a") expr
        `shouldBe` expr
    it "Remove Let with `a` in simple Let assignment" $ do
      let expr = MyLet mempty (Identifier mempty "a") (bool True) (bool True)
      removeUnused @Name @Annotation (S.singleton "a") expr
        `shouldBe` bool True
    it "Turns `a` in pattern match to PWildcard" $ do
      let expr = MyPatternMatch mempty (bool True) [(PVar mempty "a", bool True)]
          expected = MyPatternMatch mempty (bool True) [(PWildcard mempty, bool True)]
      removeUnused @Name @Annotation (S.singleton "a") expr
        `shouldBe` expected
