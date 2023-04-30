{-# LANGUAGE OverloadedStrings #-}

module Test.Typecheck.SubtypeSpec (spec) where

import Control.Monad.Writer.CPS
import Data.Either
import Data.Foldable (traverse_)
import Smol.Core
import Smol.Core.Typecheck.FromParsedExpr
import Test.Helpers
import Test.Hspec

-- Repeat after me, Duck is a subtype of Bird
-- so Duck <: Bird
-- 1 is a subtype of 1 | 2
-- so 1 <: 1 | 2
-- 1 | 2 is a subtype of Nat
-- so 1 | 2 <: Nat
-- Nat is a subtype of Int
-- so Nat <: Int
--
spec :: Spec
spec = do
  describe "Subtyping" $ do
    describe "generaliseLiteral" $ do
      it "Negative literal makes int" $ do
        generaliseLiteral (tyIntLit (-1))
          `shouldBe` TPrim () TPInt
      it "Non-negative literal makes nat" $ do
        generaliseLiteral (tyIntLit 0)
          `shouldBe` TPrim () TPNat

    describe "Subtype" $ do
      describe "Everything defeats TUnknown" $ do
        let things = [TPrim () TPBool, TVar () "horse", TFunc () mempty (TVar () "a") (TVar () "b")]
        traverse_
          ( \ty -> it (show ty <> " combines with TUnknown") $ do
              fst <$> runWriterT (isSubtypeOf ty (TUnknown () 0)) `shouldSatisfy` isRight
          )
          things

      describe "Combine two datatypes" $ do
        it "Maybe Nat <: Maybe i1" $ do
          let one = fromParsedType $ tyCons "Maybe" [tyNat]
              two = fromParsedType $ tyCons "Maybe" [tyUnknown 1]
              expected = (one, [Substitution (SubUnknown 1) (TPrim () TPNat)])

          runWriterT (one `isSubtypeOf` two)
            `shouldBe` Right expected

        it "Maybe Nat <: i1" $ do
          let one = fromParsedType $ tyCons "Maybe" [tyNat]
              two = fromParsedType $ TUnknown () 1
              expected = (one, [Substitution (SubUnknown 1) one])

          runWriterT (one `isSubtypeOf` two)
            `shouldBe` Right expected

        it "Maybe Nat <: a b" $ do
          let one = fromParsedType $ tyCons "Maybe" [tyNat]
              two = fromParsedType $ TApp () (TVar () "a") (TVar () "b")
              expected =
                ( one,
                  [ Substitution (SubId "a") (TConstructor () "Maybe"),
                    Substitution (SubId "b") (TPrim () TPNat)
                  ]
                )

          runWriterT (one `isSubtypeOf` two)
            `shouldBe` Right expected

        it "Maybe Nat <: i1 i2" $ do
          let one = fromParsedType $ tyCons "Maybe" [tyNat]
              two = fromParsedType $ TApp () (TUnknown () 1) (TUnknown () 2)
              expected =
                ( one,
                  [ Substitution (SubUnknown 1) (TConstructor () "Maybe"),
                    Substitution (SubUnknown 2) (TPrim () TPNat)
                  ]
                )

          runWriterT (one `isSubtypeOf` two)
            `shouldBe` Right expected

        it "(a -> Maybe Nat) <: (a -> i1)" $ do
          let maybeNat = tyCons "Maybe" [tyNat]
              one = fromParsedType $ TFunc () mempty (tyVar "a") maybeNat
              two = fromParsedType $ TFunc () mempty (tyVar "a") (TUnknown () 1)
              expected = (one, [Substitution (SubUnknown 1) (fromParsedType maybeNat)])

          runWriterT (one `isSubtypeOf` two)
            `shouldBe` Right expected

      describe "Combine" $ do
        let inputs =
              [ ("1", "2", "1 | 2"),
                ("1 | 2", "2", "1 | 2"),
                ("1 | 2", "3", "1 | 2 | 3")
              ]
        traverse_
          ( \(one, two, result) -> it (show one <> " <> " <> show two) $ do
              let a =
                    combine
                      (fromParsedType (unsafeParseType one))
                      (fromParsedType (unsafeParseType two))
              fst <$> runWriterT a `shouldBe` Right (fromParsedType (unsafeParseType result))
          )
          inputs

      describe "Valid pairs" $ do
        let validPairs =
              [ ("True", "True"),
                ("False", "False"),
                ("True", "Bool"),
                ("1", "a"),
                ("(True, False)", "(True,Bool)"),
                ("Maybe", "Maybe"),
                ("Maybe 1", "Maybe a"),
                ("{ item: 1 }", "{}"),
                ("[1 | 2]", "[Nat]")
              ]
        traverse_
          ( \(lhs, rhs) -> it (show lhs <> " <: " <> show rhs) $ do
              fst
                <$> runWriterT
                  ( isSubtypeOf
                      (fromParsedType $ unsafeParseType lhs)
                      (fromParsedType $ unsafeParseType rhs)
                  )
                `shouldSatisfy` isRight
          )
          validPairs
