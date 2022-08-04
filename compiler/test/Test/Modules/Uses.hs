{-# LANGUAGE OverloadedStrings #-}

module Test.Modules.Uses
  ( spec,
  )
where

import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Modules.Uses
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.Entity
import Language.Mimsa.Types.Typechecker
import Test.Hspec

extractUses' :: Expr Name (Type ()) -> Set Entity
extractUses' = extractUsesTyped

spec :: Spec
spec = do
  describe "Uses" $ do
    describe "extractUses" $ do
      it "Finds no types" $ do
        let entities = extractUses' (MyLiteral (MTPrim () MTInt) (MyInt 1))
        entities `shouldSatisfy` S.null

      it "Finds one type" $ do
        let entities = extractUses' (MyVar (MTConstructor () Nothing "Unit") Nothing "a")
        entities `shouldBe` S.fromList [EName "a", EType "Unit"]

      it "Finds one namespaced type" $ do
        let entities = extractUses' (MyVar (MTConstructor () (Just "Prelude") "Unit") Nothing "a")
        entities `shouldBe` S.fromList [EName "a", ENamespacedType "Prelude" "Unit"]

      it "Finds Either" $ do
        let expr :: Expr Name (Type Annotation)
            expr = MyLambda {expAnn = MTFunction {typAnn = None (), typArg = MTTypeApp {typAnn = Location {annStart = 129, annEnd = 132}, typFunc = MTTypeApp {typAnn = Location {annStart = 129, annEnd = 132}, typFunc = MTConstructor {typAnn = Location {annStart = 129, annEnd = 132}, typModuleName = Nothing, typTypeName = "Either"}, typArg = MTVar {typAnn = Location {annStart = 129, annEnd = 132}, typIdent = TVUnificationVar {tiUniVar = 3}}}, typArg = MTPrim {typAnn = Location {annStart = 129, annEnd = 132}, typPrim = MTBool}}, typRes = MTPrim {typAnn = Location {annStart = 135, annEnd = 148}, typPrim = MTBool}}, expBinder = Identifier {idAnn = MTTypeApp {typAnn = Location {annStart = 129, annEnd = 132}, typFunc = MTTypeApp {typAnn = Location {annStart = 129, annEnd = 132}, typFunc = MTConstructor {typAnn = Location {annStart = 129, annEnd = 132}, typModuleName = Nothing, typTypeName = "Either"}, typArg = MTVar {typAnn = Location {annStart = 129, annEnd = 132}, typIdent = TVUnificationVar {tiUniVar = 3}}}, typArg = MTPrim {typAnn = Location {annStart = 129, annEnd = 132}, typPrim = MTBool}}, idVar = "val"}, expBody = MyApp {expAnn = MTPrim {typAnn = Location {annStart = 135, annEnd = 148}, typPrim = MTBool}, expFunc = MyVar {expAnn = MTFunction {typAnn = Location {annStart = 135, annEnd = 145}, typArg = MTTypeApp {typAnn = Location {annStart = 135, annEnd = 145}, typFunc = MTTypeApp {typAnn = Location {annStart = 135, annEnd = 145}, typFunc = MTConstructor {typAnn = Location {annStart = 135, annEnd = 145}, typModuleName = Nothing, typTypeName = "Either"}, typArg = MTVar {typAnn = Location {annStart = 135, annEnd = 145}, typIdent = TVUnificationVar {tiUniVar = 3}}}, typArg = MTPrim {typAnn = Location {annStart = 135, annEnd = 145}, typPrim = MTBool}}, typRes = MTPrim {typAnn = Location {annStart = 135, annEnd = 145}, typPrim = MTBool}}, expModuleName = Nothing, expVar = "useEither"}, expArg = MyVar {expAnn = MTTypeApp {typAnn = Location {annStart = 145, annEnd = 148}, typFunc = MTTypeApp {typAnn = Location {annStart = 145, annEnd = 148}, typFunc = MTConstructor {typAnn = Location {annStart = 145, annEnd = 148}, typModuleName = Nothing, typTypeName = "Either"}, typArg = MTVar {typAnn = Location {annStart = 145, annEnd = 148}, typIdent = TVUnificationVar {tiUniVar = 3}}}, typArg = MTPrim {typAnn = Location {annStart = 145, annEnd = 148}, typPrim = MTBool}}, expModuleName = Nothing, expVar = "val"}}}
        extractUses expr `shouldBe` S.fromList [EType "Either", EName "useEither"]
