{-# LANGUAGE OverloadedStrings #-}

module Test.Modules.Uses
  ( spec,
  )
where

import qualified Data.Set as S
import Language.Mimsa.Modules.Uses
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.Entity
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "Uses" $ do
    describe "extractUses" $ do
      it "Does not include DataType declared in expression" $ do
        let expr = unsafeParseExpr "type Maybe a = Just a | Nothing; (\\val -> val : Maybe a -> Maybe a)"
            entities = extractUses expr
        entities `shouldBe` S.fromList []

    describe "extractUsesTyped" $ do
      it "Finds no types" $ do
        let entities = extractUsesTyped (MyLiteral (MTPrim () MTInt) (MyInt 1))
        entities `shouldSatisfy` S.null

      it "Finds one type" $ do
        let entities = extractUsesTyped (MyVar (MTConstructor () Nothing "Unit") Nothing "a")
        entities `shouldBe` S.fromList [EName "a", EType "Unit"]

      it "Finds one type in type app" $ do
        let entities = extractUsesTyped (MyVar (MTTypeApp () (MTConstructor () Nothing "Maybe") (MTPrim () MTInt)) Nothing "a")
        entities `shouldBe` S.fromList [EName "a", EType "Maybe"]

      it "Finds one namespaced type" $ do
        let entities = extractUsesTyped (MyVar (MTConstructor () (Just "Prelude") "Unit") Nothing "a")
        entities `shouldBe` S.fromList [EName "a", ENamespacedType "Prelude" "Unit"]

      it "Finds Either" $ do
        let expr :: Expr Name (Type ())
            expr =
              MyLambda
                { expAnn =
                    MTFunction
                      { typAnn = (),
                        typArg =
                          MTTypeApp {typAnn = (), typFunc = MTTypeApp {typAnn = (), typFunc = MTConstructor {typAnn = (), typModuleName = Nothing, typTypeName = "Either"}, typArg = MTVar {typAnn = (), typIdent = TVUnificationVar {tiUniVar = 3}}}, typArg = MTPrim {typAnn = (), typPrim = MTBool}},
                        typRes = MTPrim {typAnn = (), typPrim = MTBool}
                      },
                  expBinder = Identifier {idAnn = MTTypeApp {typAnn = (), typFunc = MTTypeApp {typAnn = (), typFunc = MTConstructor {typAnn = (), typModuleName = Nothing, typTypeName = "Either"}, typArg = MTVar {typAnn = (), typIdent = TVUnificationVar {tiUniVar = 3}}}, typArg = MTPrim {typAnn = (), typPrim = MTBool}}, idVar = "val"},
                  expBody =
                    MyApp
                      { expAnn = MTPrim {typAnn = (), typPrim = MTBool},
                        expFunc =
                          MyVar
                            { expAnn =
                                MTFunction
                                  { typAnn = (),
                                    typArg =
                                      MTTypeApp
                                        { typAnn = (),
                                          typFunc =
                                            MTTypeApp
                                              { typAnn = (),
                                                typFunc =
                                                  MTConstructor {typAnn = (), typModuleName = Nothing, typTypeName = "Either"},
                                                typArg = MTVar {typAnn = (), typIdent = TVUnificationVar {tiUniVar = 3}}
                                              },
                                          typArg =
                                            MTPrim {typAnn = (), typPrim = MTBool}
                                        },
                                    typRes = MTPrim {typAnn = (), typPrim = MTBool}
                                  },
                              expModuleName = Nothing,
                              expVar = "useEither"
                            },
                        expArg = MyVar {expAnn = MTTypeApp {typAnn = (), typFunc = MTTypeApp {typAnn = (), typFunc = MTConstructor {typAnn = (), typModuleName = Nothing, typTypeName = "Either"}, typArg = MTVar {typAnn = (), typIdent = TVUnificationVar {tiUniVar = 3}}}, typArg = MTPrim {typAnn = (), typPrim = MTBool}}, expModuleName = Nothing, expVar = "val"}
                      }
                }
        extractUsesTyped expr `shouldBe` S.fromList [EType "Either", EName "useEither"]
