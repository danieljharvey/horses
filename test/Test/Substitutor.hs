{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Substitutor
  ( spec,
  )
where

import qualified Data.Map as M
import Language.Mimsa.Store.Substitutor (substitute)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Scope
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.SubstitutedExpression
import Test.Data.Project
import Test.Helpers
import Test.Hspec

trueStoreExpr :: Monoid ann => StoreExpression ann
trueStoreExpr =
  StoreExpression (bool True) mempty mempty

falseStoreExpr :: Monoid ann => StoreExpression ann
falseStoreExpr =
  StoreExpression
    (MyVar mempty (mkName "true"))
    (Bindings $ M.singleton (mkName "true") (exprHash 1))
    mempty

constExpr :: Monoid ann => StoreExpression ann
constExpr =
  StoreExpression
    ( MyLambda
        mempty
        (mkName "a")
        ( MyLambda
            mempty
            (mkName "b")
            (MyVar mempty (mkName "a"))
        )
    )
    mempty
    mempty

maybeDecl :: DataType
maybeDecl =
  DataType (mkTyCon "Maybe") [mkName "a"] cons'
  where
    cons' =
      M.fromList
        [ (mkTyCon "Just", [VarName (mkName "a")]),
          (mkTyCon "Nothing", [])
        ]

maybeExpr :: Monoid ann => StoreExpression ann
maybeExpr =
  StoreExpression
    ( MyData
        mempty
        maybeDecl
        (MyRecord mempty mempty)
    )
    mempty
    mempty

storeWithBothIn :: Store Annotation
storeWithBothIn =
  Store
    ( M.fromList
        [ (exprHash 1, trueStoreExpr),
          (exprHash 2, falseStoreExpr),
          (exprHash 3, idExpr),
          (exprHash 4, constExpr),
          (exprHash 5, maybeExpr)
        ]
    )

testSubstitute ::
  Store Annotation ->
  StoreExpression Annotation ->
  SubstitutedExpression Annotation
testSubstitute = substitute

spec :: Spec
spec = do
  describe "Substitutor" $ do
    it "No deps, no problem" $
      do
        let ans = testSubstitute mempty trueStoreExpr
        seSwaps ans `shouldBe` mempty
        seExpr ans `shouldBe` bool True
        seScope ans `shouldBe` mempty
    it "Lambda vars are turned into numbers" $
      do
        let expr =
              MyLambda
                mempty
                (mkName "x")
                ( MyPair
                    mempty
                    (MyVar mempty (mkName "x"))
                    (MyLambda mempty (mkName "x") (MyVar mempty (mkName "x")))
                )
            expected =
              MyLambda
                mempty
                (tvFree 0)
                ( MyPair
                    mempty
                    (MyVar mempty (tvFree 0))
                    (MyLambda mempty (tvFree 1) (MyVar mempty (tvFree 1)))
                )
            expectSwaps =
              M.fromList
                [ (tvFree 0, mkName "x"),
                  (tvFree 1, mkName "x")
                ]
            ans = testSubstitute mempty (StoreExpression expr mempty mempty)
        ans `shouldBe` SubstitutedExpression expectSwaps expected mempty
    describe "One level of dep"
      $ it "Vars introduced by deps are given numbers"
      $ do
        let hash = exprHash 1
            expr = MyVar mempty (Name "exciting")
            bindings' = Bindings $ M.singleton (Name "exciting") hash
            storeExpr = StoreExpression expr bindings' mempty
            store' = Store (M.singleton hash trueStoreExpr)
            ans = testSubstitute store' storeExpr
        seSwaps ans
          `shouldBe` M.singleton (NumberedVar 0) (Name "exciting")
        seExpr ans
          `shouldBe` MyVar mempty (tvFree 0)
        seScope ans
          `shouldBe` Scope (M.singleton (NumberedVar 0) (bool True))
    describe "Only creates one new var if a function is used twice"
      $ it "let id = \\x -> x in { first: id(1), second: id(2) }"
      $ do
        let hash = exprHash 3
            expr =
              MyRecord mempty $
                M.fromList
                  [ (mkName "first", MyApp mempty (MyVar mempty (mkName "id")) (int 1)),
                    (mkName "second", MyApp mempty (MyVar mempty (mkName "id")) (int 2))
                  ]
            bindings' = Bindings $ M.singleton (mkName "id") hash
            storeExpr = StoreExpression expr bindings' mempty
            store' = Store (M.singleton hash idExpr)
            expectedId = MyLambda mempty (tvFree 0) (MyVar mempty (tvFree 0))
        let ans = testSubstitute store' storeExpr
        seSwaps ans
          `shouldBe` M.fromList
            [ (NumberedVar 0, Name "i"),
              (NumberedVar 1, Name "id")
            ]
        seExpr ans
          `shouldBe` MyRecord
            mempty
            ( M.fromList
                [ (mkName "first", MyApp mempty (MyVar mempty (tvFree 1)) (int 1)),
                  (mkName "second", MyApp mempty (MyVar mempty (tvFree 1)) (int 2))
                ]
            )
        seScope ans
          `shouldBe` Scope
            ( M.fromList
                [ (NumberedVar 1, expectedId)
                ]
            )
  describe "Combine two levels"
    $ it "'true' is introduced as a numbered variable"
    $ do
      let hash = exprHash 2
          expr = MyVar mempty (mkName "true")
          bindings' = Bindings (M.singleton (mkName "true") hash)
          storeExpr = StoreExpression expr bindings' mempty
          store' = storeWithBothIn
      let ans = testSubstitute store' storeExpr
      seSwaps ans
        `shouldBe` M.fromList
          [ (NumberedVar 0, Name "true"),
            (NumberedVar 1, Name "true")
          ]
      seExpr ans `shouldBe` MyVar mempty (tvFree 1)
      seScope ans
        `shouldBe` Scope
          ( M.fromList
              [ (NumberedVar 0, bool True),
                (NumberedVar 1, MyVar mempty (tvFree 0))
              ]
          )
  describe "Extracts types"
    $ it "Good job"
    $ do
      let hash = exprHash 5
          expr = MyLiteral mempty (MyUnit ())
          storeExpr =
            StoreExpression
              expr
              mempty
              (TypeBindings $ M.singleton (mkTyCon "Maybe") hash)
          store' = storeWithBothIn
          ans = testSubstitute store' storeExpr
      seSwaps ans `shouldBe` mempty
      seExpr ans `shouldBe` MyData mempty maybeDecl (MyLiteral mempty (MyUnit ()))
      seScope ans `shouldBe` mempty
