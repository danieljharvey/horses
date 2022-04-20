{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Typechecker.NumberVars
  ( spec,
  )
where

import Language.Mimsa.Typechecker.NumberVars
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Test.Hspec
import Test.Utils.Helpers

{-
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
-}

testAddNumbers :: StoreExpression () -> Expr Name ((), Maybe Unique)
testAddNumbers = addNumbers

spec :: Spec
spec = do
  fdescribe "NumberVars" $ do
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
                (mempty, Just 0)
                (Identifier (mempty, Nothing) "x")
                ( MyPair
                    (mempty, Nothing)
                    (MyVar (mempty, Just 0) "x")
                    ( MyLambda
                        (mempty, Just 1)
                        ( Identifier (mempty, Nothing) "x"
                        )
                        (MyVar (mempty, Just 1) "x")
                    )
                )
            ans = testAddNumbers (StoreExpression expr mempty mempty)
        ans `shouldBe` expected

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
              (mempty, Just 0)
              (Identifier (mempty, Nothing) "a")
              ( MyPatternMatch
                  (mempty, Nothing)
                  (MyLiteral (mempty, Nothing) (MyInt 1))
                  [ (PLit (mempty, Nothing) (MyInt 1), MyVar (mempty, Just 0) "a"),
                    (PWildcard (mempty, Nothing), MyVar (mempty, Just 0) "a")
                  ]
              )

          ans = testAddNumbers (StoreExpression expr mempty mempty)
      ans `shouldBe` expected

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
              (mempty, Just 0)
              (Identifier (mempty, Nothing) "a")
              ( MyPatternMatch
                  (mempty, Nothing)
                  (MyLiteral (mempty, Nothing) (MyInt 1))
                  [ (PWildcard (mempty, Nothing), MyVar (mempty, Just 0) "a"),
                    (PVar (mempty, Just 1) "a", MyVar (mempty, Just 1) "a"), -- this has a new var
                    (PWildcard (mempty, Nothing), MyVar (mempty, Just 0) "a") -- but this uses the parent one
                  ]
              )

          ans = testAddNumbers (StoreExpression expr mempty mempty)
      ans `shouldBe` expected

{-
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

-}
