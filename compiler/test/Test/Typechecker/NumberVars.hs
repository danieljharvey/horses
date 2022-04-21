{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Typechecker.NumberVars
  ( spec,
  )
where

import qualified Data.Map as M
import Language.Mimsa.Typechecker.NumberVars
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.TypeError
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Test.Hspec
import Test.Utils.Helpers

testAddNumbers ::
  StoreExpression () ->
  Either (TypeErrorF Name ()) (Expr (Name, Unique) ())
testAddNumbers = addNumbers

spec :: Spec
spec = do
  describe "NumberVars" $ do
    it "Lambda vars are turned into numbers" $
      do
        let expr =
              MyLambda
                mempty
                (Identifier mempty "x")
                ( MyPair
                    mempty
                    (MyVar mempty "x")
                    (MyLambda mempty (Identifier mempty "x") (MyVar mempty "x"))
                )
            expected =
              MyLambda
                mempty
                (Identifier mempty ("x", 0))
                ( MyPair
                    mempty
                    (MyVar mempty ("x", 0))
                    ( MyLambda
                        mempty
                        ( Identifier mempty ("x", 1)
                        )
                        (MyVar mempty ("x", 1))
                    )
                )
            ans = testAddNumbers (StoreExpression expr mempty mempty)
        ans `shouldBe` Right expected

    it "Pattern match entries work" $ do
      let expr =
            MyLambda
              mempty
              (Identifier mempty "a")
              ( MyPatternMatch
                  mempty
                  (int 1)
                  [ (PLit mempty (MyInt 1), MyVar mempty "a"),
                    (PWildcard mempty, MyVar mempty "a")
                  ]
              )
          expected =
            MyLambda
              mempty
              (Identifier mempty ("a", 0))
              ( MyPatternMatch
                  mempty
                  (MyLiteral mempty (MyInt 1))
                  [ (PLit mempty (MyInt 1), MyVar mempty ("a", 0)),
                    (PWildcard mempty, MyVar mempty ("a", 0))
                  ]
              )

          ans = testAddNumbers (StoreExpression expr mempty mempty)
      ans `shouldBe` Right expected

    it "Scoping variables in pattern matches works" $ do
      let expr =
            MyLambda
              mempty
              (Identifier mempty "a")
              ( MyPatternMatch
                  mempty
                  (int 1)
                  [ (PWildcard mempty, MyVar mempty "a"),
                    (PVar mempty "a", MyVar mempty "a"),
                    (PWildcard mempty, MyVar mempty "a")
                  ]
              )
          expected =
            MyLambda
              mempty
              (Identifier mempty ("a", 0))
              ( MyPatternMatch
                  mempty
                  (int 1)
                  [ (PWildcard mempty, MyVar mempty ("a", 0)),
                    (PVar mempty ("a", 1), MyVar mempty ("a", 1)),
                    (PWildcard mempty, MyVar mempty ("a", 0))
                  ]
              )
          ans = testAddNumbers (StoreExpression expr mempty mempty)
      ans `shouldBe` Right expected

    it "Scoping variables in let patterns works" $ do
      let expr =
            MyLambda
              mempty
              (Identifier mempty "a")
              ( MyLetPattern
                  mempty
                  (PVar mempty "a")
                  (int 1)
                  (MyVar mempty "a")
              )
          expected =
            MyLambda
              mempty
              (Identifier mempty ("a", 0))
              ( MyLetPattern
                  mempty
                  (PVar mempty ("a", 1))
                  (int 1)
                  (MyVar mempty ("a", 1))
              )

          ans = testAddNumbers (StoreExpression expr mempty mempty)
      ans `shouldBe` Right expected

    it "Fails if can't find outside dep" $ do
      let expr =
            MyVar mempty "what"
          ans = testAddNumbers (StoreExpression expr mempty mempty)
      ans `shouldBe` Left (NameNotFoundInScope mempty "what")

    it "Outside deps are assigned a number" $ do
      let expr =
            MyApp mempty (MyVar mempty "id") (MyVar mempty "id")
          expected =
            MyApp mempty (MyVar mempty ("id", 0)) (MyVar mempty ("id", 0))
          bindings = Bindings (M.singleton "id" (ExprHash "12123123"))
          ans = testAddNumbers (StoreExpression expr bindings mempty)
      ans `shouldBe` Right expected
