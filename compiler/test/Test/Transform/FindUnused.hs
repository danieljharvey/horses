{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Transform.FindUnused
  ( spec,
  )
where

import qualified Data.Set as S
import Language.Mimsa.Transform.FindUnused
import Language.Mimsa.Core
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "FindUnused" $
    do
      it "Nothing in literal" $ do
        findUnused @Annotation @Name (bool True)
          `shouldBe` mempty
      it "Finds `a` in simple Let assignment" $ do
        findUnused @Annotation @Name
          (MyLet mempty (Identifier mempty "a") (bool True) (bool True))
          `shouldBe` S.singleton ("a", mempty)
      it "Does not find `a` when it is returned later from Let" $ do
        findUnused @Annotation @Name
          (MyLet mempty (Identifier mempty "a") (bool True) (MyVar mempty Nothing "a"))
          `shouldBe` mempty
      it "Finds `a` in a pattern match" $ do
        findUnused @Annotation @Name
          (MyPatternMatch mempty (bool True) [(PVar mempty "a", bool True)])
          `shouldBe` S.singleton ("a", mempty)
      it "Finds `a` in a let pattern match" $ do
        findUnused @Annotation @Name
          (MyLetPattern mempty (PVar mempty "a") (bool True) (bool True))
          `shouldBe` S.singleton ("a", mempty)

      it "Does not find `a` when it is used in a pattern match" $ do
        findUnused @Annotation @Name
          (MyPatternMatch mempty (bool True) [(PVar mempty "a", MyVar mempty Nothing "a")])
          `shouldBe` mempty

  describe "removeUnused" $ do
    it "No change in literal" $ do
      let expr = bool True
      removeUnused @Annotation @Name expr
        `shouldBe` expr
    it "Remove Let with `a` in simple Let assignment" $ do
      let expr = MyLet mempty (Identifier mempty "a") (bool True) (bool True)
      removeUnused @Annotation @Name expr
        `shouldBe` bool True
    it "Turns `a` in pattern match to PWildcard" $ do
      let expr = MyPatternMatch mempty (bool True) [(PVar mempty "a", bool True)]
          expected = MyPatternMatch mempty (bool True) [(PWildcard mempty, bool True)]
      removeUnused @Annotation @Name expr
        `shouldBe` expected
    it "Turns `a` in let pattern match to PWildcard" $ do
      let expr = MyLetPattern mempty (PVar mempty "a") (bool True) (bool True)
          expected = MyLetPattern mempty (PWildcard mempty) (bool True) (bool True)
      removeUnused @Annotation @Name expr
        `shouldBe` expected
    it "Removes let behind a lambda" $ do
      let expr =
            MyLambda
              mempty
              (Identifier mempty "a")
              (MyLet mempty (Identifier mempty "b") (bool True) (MyVar mempty Nothing "a"))
          expected = MyLambda mempty (Identifier mempty "a") (MyVar mempty Nothing "a")
      removeUnused @Annotation @Name expr
        `shouldBe` expected
    it "Removes from broken thing" $ do
      let expr = unsafeParseExpr "let fold f total either = match either with (Left e) -> total | (Right a1) -> (f total a1); fold"
          expected = unsafeParseExpr "let fold f total either = match either with (Left _) -> total | (Right a1) -> (f total a1); fold"
      removeUnused expr `shouldBe` expected
    it "Removes from second broken thing" $ do
      let expr = unsafeParseExpr "let d = \"dog\"; \\opts -> match [ \"a\", \"b\" ] with [a, b, c] -> (Just ((a, d))) | _ -> (Nothing)"
          expected = unsafeParseExpr "let d = \"dog\"; \\opts -> match [ \"a\", \"b\" ] with [a, _, _] -> (Just ((a, d))) | _ -> (Nothing)"
      removeUnused expr `shouldBe` expected
