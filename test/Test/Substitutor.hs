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
import Test.StoreData

trueStoreExpr :: StoreExpression
trueStoreExpr = StoreExpression mempty (bool True)

falseStoreExpr :: StoreExpression
falseStoreExpr =
  StoreExpression
    (Bindings $ M.singleton (mkName "true") (ExprHash 1))
    (MyVar (mkName "true"))

constExpr :: StoreExpression
constExpr =
  StoreExpression
    mempty
    ( MyLambda
        (mkName "a")
        ( MyLambda
            (mkName "b")
            (MyVar (mkName "a"))
        )
    )

storeWithBothIn :: Store
storeWithBothIn =
  Store
    ( M.fromList
        [ (ExprHash 1, trueStoreExpr),
          (ExprHash 2, falseStoreExpr),
          (ExprHash 3, idExpr),
          (ExprHash 4, constExpr)
        ]
    )

spec :: Spec
spec = do
  describe "Substitutor" $ do
    describe "No deps, no problem"
      $ it "Just unwraps everything"
      $ substitute mempty trueStoreExpr `shouldBe` (mempty, bool True, mempty)
    describe "Leaves lambda variable alone"
      $ it "Leaves x unchanged"
      $ do
        let expr = MyLambda (mkName "x") (MyVar (mkName "x"))
            expected = MyLambda (named "x") (MyVar (named "x"))
        substitute mempty (StoreExpression mempty expr)
          `shouldBe` (mempty, expected, mempty)
    describe "Leaves built-ins alone"
      $ it "Leaves randomInt unchanged"
      $ do
        let expr = MyVar (mkName "randomInt")
            expected = MyVar (BuiltIn (mkName "randomInt"))
        substitute mempty (StoreExpression mempty expr)
          `shouldBe` (mempty, expected, mempty)
    describe "One level of dep"
      $ it "Renames the dep to var0"
      $ do
        let hash = ExprHash 1
            expr = MyVar (Name "exciting")
            bindings' = Bindings $ M.singleton (Name "exciting") hash
            storeExpr = StoreExpression bindings' expr
            store' = Store (M.singleton hash trueStoreExpr)
        substitute store' storeExpr
          `shouldBe` ( M.singleton (NumberedVar 0) (Name "exciting"),
                       MyVar (NumberedVar 0),
                       Scope (M.singleton (NumberedVar 0) (bool True))
                     )
    describe "Only creates one new var if a function is used twice"
      $ it "let id = \\x -> x in { first: id(1), second: id(2) }"
      $ do
        let hash = ExprHash 3
            expr =
              MyRecord $
                M.fromList
                  [ (mkName "first", MyApp (MyVar (mkName "id")) (int 1)),
                    (mkName "second", MyApp (MyVar (mkName "id")) (int 2))
                  ]
            bindings' = Bindings $ M.singleton (mkName "id") hash
            storeExpr = StoreExpression bindings' expr
            store' = Store (M.singleton hash idExpr)
            expectedId = MyLambda (named "i") (MyVar (named "i"))
        substitute store' storeExpr
          `shouldBe` ( M.fromList
                         [ (NumberedVar 0, Name "id")
                         ],
                       MyRecord $
                         M.fromList
                           [ (mkName "first", MyApp (MyVar (NumberedVar 0)) (int 1)),
                             (mkName "second", MyApp (MyVar (NumberedVar 0)) (int 2))
                           ],
                       Scope
                         ( M.fromList
                             [ (NumberedVar 0, expectedId)
                             ]
                         )
                     )
  describe "Combine two levels"
    $ it "Combines trueStoreExpr and falseStoreExpr"
    $ do
      let hash = ExprHash 2
          expr = MyVar (mkName "true")
          bindings' = Bindings (M.singleton (mkName "true") hash)
          storeExpr = StoreExpression bindings' expr
          store' = storeWithBothIn
      substitute store' storeExpr
        `shouldBe` ( M.fromList
                       [ (NumberedVar 0, Name "true")
                       ],
                     MyVar (NumberedVar 0),
                     Scope
                       ( M.fromList
                           [ (NumberedVar 0, bool True)
                           ]
                       )
                   )
