{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Resolver
  ( spec,
  )
where

import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Store.Resolver
import Language.Mimsa.Types
import Test.Helpers
import Test.Hspec

extractVars' :: Expr Name () -> Set Name
extractVars' = extractVars

createStoreExpression' ::
  Bindings ->
  TypeBindings ->
  Expr Name () ->
  Either ResolverError (StoreExpression ())
createStoreExpression' = createStoreExpression

createTypeStoreExpression' ::
  TypeBindings ->
  DataType ->
  Either ResolverError (StoreExpression ())
createTypeStoreExpression' = createTypeStoreExpression

spec :: Spec
spec =
  describe "Resolver" $ do
    describe "extractVars" $ do
      it "Finds none where only literals" $ do
        extractVars' (bool True) `shouldBe` mempty
        extractVars' (int 1) `shouldBe` mempty
        extractVars' (str (StringType "poo")) `shouldBe` mempty
      it "Finds a var" $
        extractVars' (MyVar mempty (Name "dog")) `shouldBe` S.singleton (Name "dog")
      it "Finds the vars in an if" $
        extractVars'
          ( MyIf
              mempty
              (MyVar mempty (Name "one"))
              (MyVar mempty (Name "two"))
              (MyVar mempty (Name "three"))
          )
          `shouldBe` S.fromList [Name "one", Name "two", Name "three"]
      it "Does not include var introduced in Let" $
        extractVars'
          ( MyLet
              mempty
              (Name "newVar")
              (MyApp mempty (MyVar mempty (Name "keep")) (int 1))
              (MyVar mempty (Name "newVar"))
          )
          `shouldBe` S.singleton (Name "keep")
      it "Does not introduce vars introduced in lambda" $
        extractVars'
          ( MyLambda
              mempty
              (Name "newVar")
              ( MyApp
                  mempty
                  (MyVar mempty (Name "keep"))
                  (MyVar mempty (Name "newVar"))
              )
          )
          `shouldBe` S.singleton (Name "keep")
    describe "createStoreExpression'" $ do
      it "Creates expressions from literals with empty Project" $ do
        createStoreExpression' mempty mempty (int 1)
          `shouldBe` Right
            ( StoreExpression
                { storeBindings = mempty,
                  storeExpression = int 1,
                  storeTypeBindings = mempty
                }
            )
        createStoreExpression' mempty mempty (bool True)
          `shouldBe` Right
            ( StoreExpression
                { storeBindings = mempty,
                  storeExpression = bool True,
                  storeTypeBindings = mempty
                }
            )
        createStoreExpression' mempty mempty (str (StringType "poo"))
          `shouldBe` Right
            ( StoreExpression
                { storeBindings = mempty,
                  storeExpression = str (StringType "poo"),
                  storeTypeBindings = mempty
                }
            )
      it "Looks for vars and can't find them" $
        createStoreExpression' mempty mempty (MyVar mempty (Name "missing"))
          `shouldBe` Left
            (MissingBinding (mkName "missing") mempty)
      it "Looks for vars and finds them" $ do
        let hash = ExprHash 1234
            expr = MyVar mempty (Name "missing")
            bindings' = Bindings $ M.singleton (Name "missing") hash
            storeExpr = createStoreExpression' bindings' mempty expr
        storeExpr
          `shouldBe` Right
            ( StoreExpression
                { storeBindings = Bindings $ M.singleton (Name "missing") hash,
                  storeExpression = expr,
                  storeTypeBindings = mempty
                }
            )
    describe "createTypeStoreExpression" $ do
      it "Creates the most basic StoreExpression for a type" $ do
        let dt = DataType (mkTyCon "Void") mempty mempty
            expr = MyData mempty dt (MyRecord mempty mempty)
            storeExpr = createStoreExpression' mempty mempty expr
        storeExpr
          `shouldBe` Right
            ( StoreExpression
                { storeBindings = mempty,
                  storeTypeBindings = mempty,
                  storeExpression = expr
                }
            )
      it "Throws when trying to use an unavailable type" $ do
        let cons' = ConsName (mkTyCon "MyUnit") []
            dt =
              DataType
                (mkTyCon "VoidBox")
                mempty
                (M.singleton (mkTyCon "Box") [cons'])
            storeExpr = createTypeStoreExpression' mempty dt
        storeExpr
          `shouldBe` Left (MissingType (mkTyCon "MyUnit") mempty)
      it "Creates a StoreExpression that uses a type from the type bindings" $ do
        let cons' = ConsName (mkTyCon "MyUnit") []
            dt =
              DataType
                (mkTyCon "VoidBox")
                mempty
                (M.singleton (mkTyCon "Box") [cons'])
            hash = ExprHash 123
            tBindings' = TypeBindings $ M.singleton (TyCon "MyUnit") hash
            storeExpr = createTypeStoreExpression' tBindings' dt
            expr = MyData mempty dt (MyRecord mempty mempty)
        storeExpr
          `shouldBe` Right
            ( StoreExpression
                { storeBindings = mempty,
                  storeTypeBindings = tBindings',
                  storeExpression = expr
                }
            )
