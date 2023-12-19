{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.TransformSpec (spec) where

import Control.Monad (void)
import Control.Monad.Identity
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Data.Text as T
import Smol.Core.Helpers
import Smol.Core.Parser
import Smol.Core.Transform
import Smol.Core.Transform.BetaReduce
import Smol.Core.Transform.ConstantFold
import Smol.Core.Transform.EtaReduce
import Smol.Core.Transform.FindUnused
import Smol.Core.Transform.FlattenLets
import Smol.Core.Transform.FloatDown
import Smol.Core.Transform.FloatUp
import Smol.Core.Transform.Inliner
import Smol.Core.Types
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = do
  describe "Transform" $ do
    describe "ConstantFolding" $ do
      let singleDefs =
            [ ("id", "id"),
              ("1 + a", "a + 1"),
              ("2 + 2", "4"),
              ("23 + 1 + a + 100 + 200", "a + 324")
            ]

      traverse_
        ( \(input, expectText) -> it ("Constant folds: " <> input) $ do
            let expr = getRight $ parseExprAndFormatError (T.pack input)
                expected = getRight $ parseExprAndFormatError (T.pack expectText)
                result = constantFold expr

            void result `shouldBe` void expected
        )
        singleDefs

    describe "BetaReduce" $ do
      let singleDefs =
            [ ("id", "id"),
              ("(\\a -> a + 1) b", "let a = b in a + 1"),
              ("(\\a -> \\b -> a + b) a1 b2", "let a = a1; let b = b2; a + b"),
              ("(\\a -> a + 1 : Int -> Int) b", "let a = b in a + 1"),
              ("if True then 1 else 2", "1"),
              ("if False then 1 else 2", "2"),
              ("{ a: 1, b: 2 }.a", "1")
            ]

      traverse_
        ( \(input, expectText) -> it ("Beta reduces: " <> input) $ do
            let expr = getRight $ parseExprAndFormatError (T.pack input)
                expected = getRight $ parseExprAndFormatError (T.pack expectText)

            void (betaReduce expr) `shouldBe` void expected
        )
        singleDefs

    describe "EtaReduce" $ do
      let singleDefs =
            [ ("id", "id"),
              ("\\a -> id a", "id"),
              ("\\a -> \\b -> id a b", "id"),
              ("\\a -> \\b -> \\c -> id a b c", "id")
            ]

      traverse_
        ( \(input, expectText) -> it ("Eta reduces: " <> input) $ do
            let expr = getRight $ parseExprAndFormatError (T.pack input)
                expected = getRight $ parseExprAndFormatError (T.pack expectText)

            void (etaReduce expr) `shouldBe` void expected
        )
        singleDefs

    describe "FlattenLets" $ do
      let singleDefs =
            [ ("id", "id"),
              ( "let a = (let b = 1 in b + 1) in a + 1",
                "let b = 1; let a = b + 1; a + 1"
              ),
              ( "let a = (let b = (let c = 1 in c + 1) in b + 1) in a + 1",
                "let c = 1; let b = c + 1; let a = b + 1; a + 1"
              )
            ]

      traverse_
        ( \(input, expectText) -> it ("Flattens lets: " <> input) $ do
            let expr = getRight $ parseExprAndFormatError (T.pack input)
                expected = getRight $ parseExprAndFormatError (T.pack expectText)

            void (flattenLets expr) `shouldBe` void expected
        )
        singleDefs

    describe "FloatDown" $ do
      let singleDefs =
            [ ("id", "id"),
              ( "let a = 1; case a { True -> 1, False -> 2 }",
                "let a = 1; case a { True -> 1, False -> 2 }"
              ),
              ( "let a = 1; case b { True -> 1, False -> 2 }",
                "case b { True -> let a = 1; 1, False -> let a = 1; 2}"
              ),
              ("let a = 1; if a then 1 else 2", "let a = 1; if a then 1 else 2"),
              ( "let a = 1; if b then 1 else 2",
                "if b then let a = 1; 1 else let a = 1; 2"
              ),
              ( "let a = 1; let b = 2; if c then 1 else 2",
                "if c then let a = 1; let b = 2; 1 else let a = 1; let b = 2; 2"
              )
            ]

      traverse_
        ( \(input, expectText) -> it ("Flattens lets: " <> input) $ do
            let expr = getRight $ parseExprAndFormatError (T.pack input)
                expected = getRight $ parseExprAndFormatError (T.pack expectText)

            void (floatDown expr) `shouldBe` void expected
        )
        singleDefs

    describe "FindUnused" $
      do
        it "Nothing in literal" $ do
          findUnused @Annotation @Identity (bool True)
            `shouldBe` mempty

        it "Finds `a` in simple Let assignment" $ do
          findUnused @Annotation @Identity
            (ELet mempty (pure "a") (bool True) (bool True))
            `shouldBe` S.singleton ("a", mempty)

        it "Does not find `a` when it is returned later from Let" $ do
          findUnused @Annotation @Identity
            (ELet mempty (pure "a") (bool True) (EVar mempty "a"))
            `shouldBe` mempty

        it "Finds `a` in a pattern match" $ do
          findUnused @Annotation @Identity
            (EPatternMatch mempty (bool True) (NE.fromList [(PVar mempty (pure "a"), bool True)]))
            `shouldBe` S.singleton ("a", mempty)

        it "Does not find `a` when it is used in a pattern match" $ do
          findUnused @Annotation @Identity
            (EPatternMatch mempty (bool True) (NE.fromList [(PVar mempty "a", EVar mempty "a")]))
            `shouldBe` mempty

    describe "removeUnused" $ do
      it "No change in literal" $ do
        let expr = bool True
        removeUnused @Annotation @Identity expr
          `shouldBe` expr

      it "Remove Let with `a` in simple Let assignment" $ do
        let expr = ELet mempty (pure "a") (bool True) (bool True)
        removeUnused @Annotation @Identity expr
          `shouldBe` bool True

      it "Turns `a` in pattern match to PWildcard" $ do
        let expr = EPatternMatch mempty (bool True) (NE.fromList [(PVar mempty "a", bool True)])
            expected = EPatternMatch mempty (bool True) (NE.fromList [(PWildcard mempty, bool True)])
        removeUnused @Annotation @Identity expr
          `shouldBe` expected

      it "Removes let behind a lambda" $ do
        let expr =
              ELambda
                mempty
                (pure "a")
                (ELet mempty (pure "b") (bool True) (EVar mempty "a"))
            expected = ELambda mempty (pure "a") (EVar mempty "a")
        removeUnused @Annotation @Identity expr
          `shouldBe` expected

      it "Removes from broken thing" $ do
        let expr = unsafeParseExpr "let fold f total either = case either {(Left e) -> total, (Right a1) -> (f total a1)}; fold"
            expected = unsafeParseExpr "let fold f total either = case either {(Left _) -> total, (Right a1) -> (f total a1)}; fold"
        removeUnused expr `shouldBe` expected

      it "Removes from second broken thing" $ do
        let expr = unsafeParseExpr "let d = \"dog\"; \\opts -> case [ \"a\", \"b\" ] { [a, b, c] -> (Just ((a, d))), _ -> (Nothing)}"
            expected = unsafeParseExpr "let d = \"dog\"; \\opts -> case [ \"a\", \"b\" ] { [a, _, _] -> (Just ((a, d))), _ -> (Nothing) }"
        removeUnused expr `shouldBe` expected

    describe "FloatUp" $ do
      it "Does nothing when no pattern match" $ do
        let expr = unsafeParseExpr "let a = True in 1"
        floatUp expr `shouldBe` expr
      it "Does nothing when let uses lambda variable" $ do
        let expr = unsafeParseExpr "\\b -> let a = b + 1; a + b"
        floatUp expr `shouldBe` expr
      it "Pushes a let above a lambda" $ do
        let expr = unsafeParseExpr "\\b -> let a = 1; a + b"
            expected = unsafeParseExpr "let a = 1; \\b -> a + b"
        floatUp expr `shouldBe` expected
      it "Pushes a let up once but not twice" $ do
        let expr = unsafeParseExpr "\\a -> \\c -> let b = a + 1; a + b + c"
            expected = unsafeParseExpr "\\a -> let b = a + 1; \\c -> a + b + c"
        floatUp expr `shouldBe` expected
      it "Pushes a let up twice" $ do
        let expr = unsafeParseExpr "\\b -> \\c -> let a = 1; a + b + c"
            expected = unsafeParseExpr "let a = 1; \\b -> \\c -> a + b + c"
        floatUp expr `shouldBe` expected

    describe "Inliner" $ do
      describe "howTrivial" $ do
        it "Yes to number literal" $ do
          howTrivial (unsafeParseExpr "1")
            `shouldSatisfy` isJust
        it "Yes to string literal" $ do
          howTrivial (unsafeParseExpr "\"dog\"")
            `shouldSatisfy` isJust
        it "Yes to bool literal" $ do
          howTrivial (unsafeParseExpr "True")
            `shouldSatisfy` isJust
        it "Yes to number array literal" $
          do
            howTrivial (unsafeParseExpr "[1,2,3]")
            `shouldSatisfy` isJust
        it "Yes to record full of literals" $ do
          howTrivial (unsafeParseExpr "{ a: 1, b: True, c: \"dog\", d: [1,2,3] }")
            `shouldSatisfy` isJust
        it "Yes to var" $ do
          howTrivial (unsafeParseExpr "b")
            `shouldSatisfy` isJust
        it "No to function" $ do
          howTrivial (unsafeParseExpr "\\a -> True")
            `shouldBe` Nothing

      describe "inlineInternal" $ do
        let inlineInternal' = inlineInternal (InlineState mempty)
        it "Does nothing when no vars" $ do
          let expr = unsafeParseExpr "True"
          inlineInternal' expr
            `shouldBe` expr
        it "Inlines simple literal that is used once" $ do
          let expr = unsafeParseExpr "let a = 1 in a"
              expected = unsafeParseExpr "let a = 1 in 1"
          inlineInternal' expr
            `shouldBe` expected
        it "Inline function when it is used once" $ do
          let expr = unsafeParseExpr "let a = \\b -> 1 in a"
              expected = unsafeParseExpr "let a = \\b -> 1 in \\b -> 1"
          inlineInternal' expr
            `shouldBe` expected
        it "Does not inlines trivial item into function if it used more than once" $ do
          let expr = unsafeParseExpr "let a = 1 in \\f -> g True a a"
          inlineInternal' expr
            `shouldBe` expr
        it "Does not inline recursive definition" $ do
          let expr = unsafeParseExpr "let flip as = if as then False else flip as in flip False"
          inlineInternal' expr
            `shouldBe` expr

    describe "Whole transform" $ do
      let singleDefs =
            [ ( "let eqint = (\\a -> \\b -> a == b : Int -> Int -> Bool); eqint (1 : Int) (2 : Int)",
                "(1 : Int) == (2 : Int)"
              ),
              ( "(\\a -> a + 1) (b : Int)",
                "(b : Int) + 1"
              ),
              ( "let shownatural = (\\nat -> \"Oh\" : Natural -> String); shownatural (Suc Zero)",
                "\"Oh\""
              ),
              ( "let shownatural = (\\nat -> case nat { Suc _ -> \"Oh\", _ -> \"\" } : Natural -> String); shownatural (Suc Zero)",
                "case (Suc Zero) { Suc _ -> \"Oh\", _ -> \"\" }"
              ),
              ( "let addMany = (\\a -> 1 + 123 + a + 123 : Int -> Int); addMany 1",
                "248"
              )
            ]
      traverse_
        ( \(inputExpr, inputExpected) -> it ("Transform " <> show inputExpr) $ do
            let expr = unsafeParseExpr inputExpr
                expected = unsafeParseExpr inputExpected
            let result = transform expr

            result `shouldBe` expected
        )
        singleDefs
