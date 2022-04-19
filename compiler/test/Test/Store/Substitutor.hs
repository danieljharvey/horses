{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Store.Substitutor
  ( spec,
  )
where

import qualified Data.Map as M
import Language.Mimsa.Store.Substitutor (substitute)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.SubstitutedExpression
import Language.Mimsa.Types.Typechecker.MonoType
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

trueStoreExpr :: Monoid ann => StoreExpression ann
trueStoreExpr =
  StoreExpression (bool True) mempty mempty

falseStoreExpr :: Monoid ann => StoreExpression ann
falseStoreExpr =
  StoreExpression
    (MyVar mempty "true")
    (Bindings $ M.singleton "true" (exprHash 1))
    mempty

constExpr :: Monoid ann => StoreExpression ann
constExpr =
  StoreExpression
    ( MyLambda
        mempty
        (Identifier mempty "a")
        ( MyLambda
            mempty
            (Identifier mempty "b")
            (MyVar mempty "a")
        )
    )
    mempty
    mempty

maybeDecl :: DataType
maybeDecl =
  DataType "Maybe" ["a"] cons'
  where
    cons' =
      M.fromList
        [ ("Just", [MTVar mempty (tvNamed "a")]),
          ("Nothing", [])
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
                (Identifier mempty $ numbered 0)
                ( MyPair
                    mempty
                    (MyVar mempty (numbered 0))
                    (MyLambda mempty (Identifier mempty $ numbered 1) (MyVar mempty (numbered 1)))
                )
            expectSwaps =
              M.fromList
                [ (numbered 0, "x"),
                  (numbered 1, "x")
                ]
            ans = testSubstitute mempty (StoreExpression expr mempty mempty)
        ans `shouldBe` SubstitutedExpression expectSwaps expected
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
              (Identifier mempty $ numbered 0)
              ( MyPatternMatch
                  mempty
                  (int 1)
                  [ (PLit mempty (MyInt 1), MyVar mempty (numbered 0)),
                    ( PWildcard mempty,
                      MyVar mempty (numbered 0)
                    )
                  ]
              )
          expectSwaps = M.singleton (numbered 0) "a"
          ans = testSubstitute mempty (StoreExpression expr mempty mempty)
      ans `shouldBe` SubstitutedExpression expectSwaps expected
    describe "One level of dep" $
      it "Vars introduced by deps are given numbers" $
        do
          let hash = exprHash 1
              expr = MyVar mempty (Name "exciting")
              bindings' = Bindings $ M.singleton (Name "exciting") hash
              storeExpr = StoreExpression expr bindings' mempty
              store' = Store (M.singleton hash trueStoreExpr)
              ans = testSubstitute store' storeExpr
          seSwaps ans
            `shouldBe` M.singleton (NumberedVar 0) (Name "exciting")
          seExpr ans
            `shouldBe` MyVar mempty (numbered 0)
    describe "Only creates one new var if a function is used twice" $
      it "let id = \\x -> x in { first: id(1), second: id(2) }" $
        do
          let hash = exprHash 3
              expr =
                MyRecord mempty $
                  M.fromList
                    [ ("first", MyApp mempty (MyVar mempty "id") (int 1)),
                      ("second", MyApp mempty (MyVar mempty "id") (int 2))
                    ]
              bindings' = Bindings $ M.singleton "id" hash
              storeExpr = StoreExpression expr bindings' mempty
              store' = Store (M.singleton hash idExpr)
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
                  [ ("first", MyApp mempty (MyVar mempty (numbered 1)) (int 1)),
                    ("second", MyApp mempty (MyVar mempty (numbered 1)) (int 2))
                  ]
              )

  describe "Combine two levels" $
    it "'true' is introduced as a numbered variable" $
      do
        let hash = exprHash 2
            expr = MyVar mempty "true"
            bindings' = Bindings (M.singleton "true" hash)
            storeExpr = StoreExpression expr bindings' mempty
            store' = storeWithBothIn
        let ans = testSubstitute store' storeExpr
        seSwaps ans
          `shouldBe` M.fromList
            [ (NumberedVar 0, Name "true"),
              (NumberedVar 1, Name "true")
            ]
        seExpr ans `shouldBe` MyVar mempty (numbered 1)
  describe "Extracts types" $
    it "Good job" $
      do
        let hash = exprHash 5
            expr = MyLiteral mempty (MyBool True)
            storeExpr =
              StoreExpression
                expr
                mempty
                (TypeBindings $ M.singleton "Maybe" hash)
            store' = storeWithBothIn
            ans = testSubstitute store' storeExpr
        seSwaps ans `shouldBe` mempty
        seExpr ans `shouldBe` MyLiteral mempty (MyBool True)
