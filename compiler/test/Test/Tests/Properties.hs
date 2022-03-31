{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tests.Properties
  ( spec,
  )
where

import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Either
import Data.Functor
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Tests.Generate
import Language.Mimsa.Tests.Helpers
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Typechecker.Typecheck
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

getStoreExprs :: Project Annotation -> S.Set (StoreExpression Annotation)
getStoreExprs =
  S.fromList
    . M.elems
    . getStore
    . prjStore

itTypeChecks :: MonoType -> Expr Name Annotation -> Either TypeError ()
itTypeChecks mt expr = do
  let elabbed =
        fmap (\(_, _, a, _) -> a)
          . typecheck
            mempty
            mempty
            (createEnv mempty (getStoreExprs testStdlib))
          $ first NamedVar expr
  generatedMt <- getTypeFromAnn <$> elabbed
  unifies mt generatedMt

itGenerates :: MonoType -> Expectation
itGenerates mt = do
  samples <- liftIO $ generateFromMonoType @() (getStoreExprs testStdlib) mt
  let success = traverse (itTypeChecks mt) (fmap ($> mempty) samples)
  success `shouldSatisfy` isRight

spec :: Spec
spec = do
  describe "Properties" $ do
    describe "Test the testing" $ do
      it "typechecking check works" $ do
        itTypeChecks (MTPrim mempty MTInt) (MyLiteral mempty (MyInt 100))
          `shouldSatisfy` isRight
      it "typechecking fail works" $ do
        itTypeChecks (MTPrim mempty MTBool) (MyLiteral mempty (MyInt 100))
          `shouldSatisfy` isLeft
    describe "isRecursive" $ do
      it "is not recursive" $ do
        isRecursive "Unit" [] `shouldBe` False
      it "is not recursive 2" $ do
        isRecursive "Maybe" [MTPrim mempty MTInt] `shouldBe` False
      it "is recursive" $ do
        isRecursive
          "List"
          [ MTTypeApp mempty (MTConstructor mempty "List") (MTPrim mempty MTInt)
          ]
          `shouldBe` True

    describe "Test generators" $ do
      it "Bool" $ do
        itGenerates mtBool
      it "Int" $ do
        itGenerates mtInt
      it "String" $ do
        itGenerates mtString
      it "Array of ints" $ do
        itGenerates (MTArray mempty mtInt)
      it "Pair of int and string" $ do
        itGenerates (MTPair mempty mtInt mtString)
      it "Records" $ do
        let record = MTRecord mempty (M.fromList [("dog", mtInt), ("cat", mtBool)])
        itGenerates record
      it "Functions" $ do
        itGenerates (MTFunction mempty mtBool mtInt)
      it "Nested functions" $ do
        itGenerates (MTFunction mempty mtString (MTFunction mempty mtBool mtInt))
      it "Constructor" $ do
        itGenerates (MTConstructor mempty "TrafficLight")
      it "Constructor with var" $ do
        itGenerates (MTTypeApp mempty (MTConstructor mempty "Maybe") mtInt)
      it "Constructor with nested vars" $ do
        itGenerates (MTTypeApp mempty (MTConstructor mempty "Tree") mtBool)
