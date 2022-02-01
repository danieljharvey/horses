{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Transform.FindUnused
  ( spec,
  )
where

import qualified Data.Set as S
import Language.Mimsa.Transform.FindUnused
import Language.Mimsa.Types.AST
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "FindUnused" $
    do
      it "Nothing in literal" $ do
        findUnused @Annotation (bool True)
          `shouldBe` mempty
      it "Finds `a` in simple Let assignment" $ do
        findUnused @Annotation
          (MyLet mempty (Identifier mempty (named "a")) (bool True) (bool True))
          `shouldBe` S.singleton (named "a", mempty)
      it "Does not find `a` when it is returned later from Let" $ do
        findUnused @Annotation
          (MyLet mempty (Identifier mempty (named "a")) (bool True) (MyVar mempty (named "a")))
          `shouldBe` mempty
      it "Finds `a` in a pattern match" $ do
        findUnused @Annotation
          (MyPatternMatch mempty (bool True) [(PVar mempty (named "a"), bool True)])
          `shouldBe` S.singleton (named "a", mempty)
      it "Finds `a` in a let pattern match" $ do
        findUnused @Annotation
          (MyLetPattern mempty (PVar mempty (named "a")) (bool True) (bool True))
          `shouldBe` S.singleton (named "a", mempty)

      it "Does not find `a` when it is used in a pattern match" $ do
        findUnused @Annotation
          (MyPatternMatch mempty (bool True) [(PVar mempty (named "a"), MyVar mempty (named "a"))])
          `shouldBe` mempty

  describe "removeUnused" $ do
    it "No change in literal" $ do
      let expr = bool True
      removeUnused @Annotation (S.singleton (named "a")) expr
        `shouldBe` expr
    it "Remove Let with `a` in simple Let assignment" $ do
      let expr = MyLet mempty (Identifier mempty (named "a")) (bool True) (bool True)
      removeUnused @Annotation (S.singleton (named "a")) expr
        `shouldBe` bool True
    it "Turns `a` in pattern match to PWildcard" $ do
      let expr = MyPatternMatch mempty (bool True) [(PVar mempty (named "a"), bool True)]
          expected = MyPatternMatch mempty (bool True) [(PWildcard mempty, bool True)]
      removeUnused @Annotation (S.singleton (named "a")) expr
        `shouldBe` expected
    it "Turns `a` in let pattern match to PWildcard" $ do
      let expr = MyLetPattern mempty (PVar mempty (named "a")) (bool True) (bool True)
          expected = MyLetPattern mempty (PWildcard mempty) (bool True) (bool True)
      removeUnused @Annotation (S.singleton (named "a")) expr
        `shouldBe` expected
    it "Removes let behind a lambda" $ do
      let expr =
            MyLambda
              mempty
              (Identifier mempty (named "a"))
              (MyLet mempty (Identifier mempty (named "b")) (bool True) (MyVar mempty (named "a")))
          expected = MyLambda mempty (Identifier mempty (named "a")) (MyVar mempty (named "a"))
      removeUnused @Annotation (S.singleton (named "b")) expr
        `shouldBe` expected
