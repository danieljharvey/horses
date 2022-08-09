{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Typechecker.NumberVars
  ( spec,
  )
where

import Data.Either
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Typechecker.NumberVars
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.TypeError
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.ModuleHash
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker.Unique
import Test.Hspec
import Test.Utils.Helpers

testAddNumbers ::
  StoreExpression () ->
  Either (TypeErrorF Name ()) (Expr (Name, Unique) ())
testAddNumbers = addNumbersToStoreExpression

testAddToExpr ::
  Expr Name () ->
  Either (TypeErrorF Name ()) (Expr (Name, Unique) ())
testAddToExpr = addNumbersToExpression mempty mempty mapMods
  where
    mapMods = M.singleton "Prelude" (ModuleHash "123", S.singleton "id")

spec :: Spec
spec = do
  describe "NumberVars" $ do
    describe "Expression" $ do
      it "Normal var" $ do
        let expr =
              MyLambda
                mempty
                (Identifier mempty "x")
                (MyVar mempty Nothing "x")
            expected =
              MyLambda
                mempty
                (Identifier mempty ("x", Unique 0))
                ( MyVar mempty Nothing ("x", Unique 0)
                )
        testAddToExpr expr `shouldBe` Right expected

      it "Namespaced var" $ do
        let expr = MyVar mempty (Just "Prelude") "id"
            expected =
              MyVar
                mempty
                (Just "Prelude")
                ("id", ModuleDep (ModuleHash "123"))
        testAddToExpr expr `shouldBe` Right expected

    describe "StoreExpression" $ do
      it "Lambda vars are turned into numbers" $
        do
          let expr =
                MyLambda
                  mempty
                  (Identifier mempty "x")
                  ( MyPair
                      mempty
                      (MyVar mempty Nothing "x")
                      (MyLambda mempty (Identifier mempty "x") (MyVar mempty Nothing "x"))
                  )
              expected =
                MyLambda
                  mempty
                  (Identifier mempty ("x", Unique 0))
                  ( MyPair
                      mempty
                      (MyVar mempty Nothing ("x", Unique 0))
                      ( MyLambda
                          mempty
                          ( Identifier mempty ("x", Unique 1)
                          )
                          (MyVar mempty Nothing ("x", Unique 1))
                      )
                  )
              ans = testAddNumbers (StoreExpression expr mempty mempty mempty mempty)
          ans `shouldBe` Right expected

      it "Pattern match entries work" $ do
        let expr =
              MyLambda
                mempty
                (Identifier mempty "a")
                ( MyPatternMatch
                    mempty
                    (int 1)
                    [ (PLit mempty (MyInt 1), MyVar mempty Nothing "a"),
                      (PWildcard mempty, MyVar mempty Nothing "a")
                    ]
                )
            expected =
              MyLambda
                mempty
                (Identifier mempty ("a", Unique 0))
                ( MyPatternMatch
                    mempty
                    (MyLiteral mempty (MyInt 1))
                    [ (PLit mempty (MyInt 1), MyVar mempty Nothing ("a", Unique 0)),
                      (PWildcard mempty, MyVar mempty Nothing ("a", Unique 0))
                    ]
                )

            ans = testAddNumbers (StoreExpression expr mempty mempty mempty mempty)
        ans `shouldBe` Right expected

      it "Scoping variables in pattern matches works" $ do
        let expr =
              MyLambda
                mempty
                (Identifier mempty "a")
                ( MyPatternMatch
                    mempty
                    (int 1)
                    [ (PWildcard mempty, MyVar mempty Nothing "a"),
                      (PVar mempty "a", MyVar mempty Nothing "a"),
                      (PWildcard mempty, MyVar mempty Nothing "a")
                    ]
                )
            expected =
              MyLambda
                mempty
                (Identifier mempty ("a", Unique 0))
                ( MyPatternMatch
                    mempty
                    (int 1)
                    [ (PWildcard mempty, MyVar mempty Nothing ("a", Unique 0)),
                      (PVar mempty ("a", Unique 1), MyVar mempty Nothing ("a", Unique 1)),
                      (PWildcard mempty, MyVar mempty Nothing ("a", Unique 0))
                    ]
                )
            ans = testAddNumbers (StoreExpression expr mempty mempty mempty mempty)
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
                    (MyVar mempty Nothing "a")
                )
            expected =
              MyLambda
                mempty
                (Identifier mempty ("a", Unique 0))
                ( MyLetPattern
                    mempty
                    (PVar mempty ("a", Unique 1))
                    (int 1)
                    (MyVar mempty Nothing ("a", Unique 1))
                )

            ans = testAddNumbers (StoreExpression expr mempty mempty mempty mempty)
        ans `shouldBe` Right expected

      it "Does not explode with a namespaced dep" $ do
        let expr =
              MyVar mempty (Just "Prelude") "what"
            valueDeps = M.singleton (Just "Prelude", "what") (ExprHash "13")
            ans = testAddNumbers (StoreExpression expr valueDeps mempty mempty mempty)
        ans `shouldSatisfy` isRight

      it "Fails if can't find outside dep" $ do
        let expr =
              MyVar mempty Nothing "what"
            ans = testAddNumbers (StoreExpression expr mempty mempty mempty mempty)
        ans `shouldBe` Left (NameNotFoundInScope mempty mempty Nothing "what")

      it "Outside deps are assigned a number" $ do
        let hash = ExprHash "123"
        let expr =
              MyApp mempty (MyVar mempty Nothing "id") (MyVar mempty Nothing "id")
            expected =
              MyApp
                mempty
                (MyVar mempty Nothing ("id", Dependency hash))
                (MyVar mempty Nothing ("id", Dependency hash))
            bindings = M.singleton (Nothing, "id") hash
            ans = testAddNumbers (StoreExpression expr bindings mempty mempty mempty)
        ans `shouldBe` Right expected
