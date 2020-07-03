{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Substitutor
  ( spec,
  )
where

import qualified Data.Map as M
import Language.Mimsa.Store.Substitutor (substitute)
import Language.Mimsa.Types
import Test.Helpers
import Test.Hspec

trueStoreExpr :: StoreExpression
trueStoreExpr = StoreExpression mempty (bool True)

falseStoreExpr :: StoreExpression
falseStoreExpr =
  StoreExpression
    (Bindings $ M.singleton (mkName "true") (ExprHash 1))
    (MyVar (mkName "true"))

idExpr :: StoreExpression
idExpr = StoreExpression (mempty) (MyLambda (mkName "a") (MyVar (mkName "a")))

storeWithBothIn :: Store
storeWithBothIn =
  Store
    ( M.fromList
        [ (ExprHash 1, trueStoreExpr),
          (ExprHash 2, falseStoreExpr),
          (ExprHash 3, idExpr)
        ]
    )

spec :: Spec
spec = do
  describe "Substitutor" $ do
    describe "No deps, no problem" $ do
      it "Just unwraps everything" $ do
        substitute mempty trueStoreExpr `shouldBe` Right (mempty, (storeExpression trueStoreExpr), mempty)
    describe "Leaves lambda variable alone" $ do
      it "Leaves x unchanged" $ do
        let expr = MyLambda (mkName "x") (MyVar (mkName "x"))
        substitute mempty (StoreExpression mempty expr)
          `shouldBe` Right (mempty, expr, mempty)
    describe "Leaves built-ins alone" $ do
      it "Leaves randomInt unchanged" $ do
        let expr = (MyVar (mkName "randomInt"))
        substitute mempty (StoreExpression mempty expr)
          `shouldBe` Right (mempty, expr, mempty)
    describe "One level of dep" $ do
      it "Renames the dep to var0" $ do
        let hash = ExprHash 1
            expr = MyVar (Name "exciting")
            bindings' = Bindings $ M.singleton (Name "exciting") hash
            storeExpr = StoreExpression bindings' expr
            store' = Store (M.singleton hash trueStoreExpr)
        substitute store' storeExpr
          `shouldBe` Right
            ( M.singleton (Name "var0") (Name "exciting"),
              (MyVar (Name "var0")),
              Scope (M.singleton (Name "var0") (storeExpression trueStoreExpr))
            )
    describe "Only creates one new var if a function is used twice" $ do
      it "let id = \\x -> x in { first: id(1), second: id(2) }" $ do
        let hash = ExprHash 3
            expr =
              ( MyRecord $
                  M.fromList
                    [ (mkName "first", MyApp (MyVar (mkName "id")) (int 1)),
                      (mkName "second", MyApp (MyVar (mkName "id")) (int 2))
                    ]
              )
            bindings' = Bindings $ M.singleton (mkName "id") hash
            storeExpr = StoreExpression bindings' expr
            store' = Store (M.singleton hash idExpr)
        substitute store' storeExpr
          `shouldBe` Right
            ( M.fromList
                [ (Name "var0", Name "id"),
                  (Name "var1", Name "id")
                ],
              ( MyRecord $
                  M.fromList
                    [ (mkName "first", MyApp (MyVar (mkName "var0")) (int 1)),
                      (mkName "second", MyApp (MyVar (mkName "var1")) (int 2))
                    ]
              ),
              Scope
                ( M.fromList
                    [ (Name "var0", storeExpression idExpr),
                      (Name "var1", storeExpression idExpr)
                    ]
                )
            )
  describe "Combine two levels" $ do
    it "Combines trueStoreExpr and falseStoreExpr" $ do
      let hash = ExprHash 2
          expr = MyVar (mkName "true")
          bindings' = Bindings (M.singleton (mkName "true") hash)
          storeExpr = StoreExpression bindings' expr
          store' = storeWithBothIn
      substitute store' storeExpr
        `shouldBe` Right
          ( M.fromList
              [ (Name "var0", Name "true"),
                (Name "var1", Name "true")
              ],
            MyVar (mkName "var0"),
            Scope
              ( M.fromList
                  [ (Name "true", bool True),
                    (Name "var1", MyVar (mkName "true")),
                    (Name "var0", MyVar (mkName "var1"))
                  ]
              )
          )
