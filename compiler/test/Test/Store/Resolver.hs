{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Store.Resolver
  ( spec,
  )
where

import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Store.Resolver
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Test.Hspec
import Test.Utils.Helpers

extractVars' :: Expr Name () -> Set Name
extractVars' = extractVars

createStoreExpression' ::
  Bindings ->
  TypeBindings ->
  Expr Name () ->
  Either ResolverError (StoreExpression ())
createStoreExpression' = createStoreExpression

spec :: Spec
spec =
  describe "Resolver" $ do
    describe "extractVars" $ do
      it "Finds none where only literals" $ do
        extractVars' (bool True) `shouldBe` mempty
        extractVars' (int 1) `shouldBe` mempty
        extractVars' (str (StringType "poo")) `shouldBe` mempty
      it "Finds a var" $
        extractVars' (MyVar mempty Nothing (Name "dog")) `shouldBe` S.singleton (Name "dog")
      it "Finds the vars in an if" $
        extractVars'
          ( MyIf
              mempty
              (MyVar mempty Nothing (Name "one"))
              (MyVar mempty Nothing (Name "two"))
              (MyVar mempty Nothing (Name "three"))
          )
          `shouldBe` S.fromList [Name "one", Name "two", Name "three"]
      it "Does not include var introduced in Let" $
        extractVars'
          ( MyLet
              mempty
              (Identifier mempty (Name "newVar"))
              (MyApp mempty (MyVar mempty Nothing (Name "keep")) (int 1))
              (MyVar mempty Nothing (Name "newVar"))
          )
          `shouldBe` S.singleton (Name "keep")
      it "Does not introduce vars introduced in lambda" $
        extractVars'
          ( MyLambda
              mempty
              (Identifier mempty $ Name "newVar")
              ( MyApp
                  mempty
                  (MyVar mempty Nothing (Name "keep"))
                  (MyVar mempty Nothing (Name "newVar"))
              )
          )
          `shouldBe` S.singleton (Name "keep")
    describe "createStoreExpression'" $ do
      it "Creates expressions from literals with empty Project" $ do
        createStoreExpression' mempty mempty (int 1)
          `shouldBe` Right
            ( mkStoreExpression (int 1)
            )
        createStoreExpression' mempty mempty (bool True)
          `shouldBe` Right
            (mkStoreExpression (bool True))
        createStoreExpression' mempty mempty (str (StringType "poo"))
          `shouldBe` Right
            ( mkStoreExpression (str (StringType "poo"))
            )
      it "Looks for vars and can't find them" $
        createStoreExpression' mempty mempty (MyVar mempty Nothing (Name "missing"))
          `shouldBe` Left
            (MissingBinding "missing" mempty)
      it "Looks for vars and finds them" $ do
        let hash = exprHash 1234
            expr = MyVar mempty Nothing (Name "missing")
            bindings' = Bindings $ M.singleton (Name "missing") hash
            storeExpr = createStoreExpression' bindings' mempty expr
        storeExpr
          `shouldBe` Right
            ( StoreExpression
                { storeBindings = M.singleton (Nothing, Name "missing") hash,
                  storeExpression = expr,
                  storeTypeBindings = mempty,
                  storeInfixes = mempty,
                  storeTypes = mempty
                }
            )
    describe "createTypeStoreExpression" $ do
      it "Creates the most basic StoreExpression for a type" $ do
        let dt = DataType "Void" mempty mempty
            expr = MyData mempty dt (MyRecord mempty mempty)
            storeExpr = createStoreExpression' mempty mempty expr
        storeExpr
          `shouldBe` Right
            ( StoreExpression
                { storeBindings = mempty,
                  storeTypeBindings = mempty,
                  storeExpression = expr,
                  storeInfixes = mempty,
                  storeTypes = mempty
                }
            )
