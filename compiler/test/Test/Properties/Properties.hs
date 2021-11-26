{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.Properties
  ( spec,
  )
where

import Control.Monad.IO.Class
import Data.Functor
import Language.Mimsa.Properties.Generate
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Typechecker.Typecheck
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers

itTypeChecks :: MonoType -> Expr Variable Annotation -> Either TypeError Bool
itTypeChecks mt expr =
  let elabbed =
        fmap (\(_, _, a, _) -> a)
          . typecheck mempty mempty mempty
          $ expr
   in (mt ==) . getTypeFromAnn <$> elabbed

itGenerates :: MonoType -> Expectation
itGenerates mt = do
  samples <- liftIO $ generateFromMonoType mt
  let success = traverse (itTypeChecks mt) (fmap ($> mempty) samples)
  and <$> success `shouldBe` Right True

spec :: Spec
spec = do
  fdescribe "Properties" $ do
    describe "Test the testing" $ do
      it "typechecking check works" $ do
        itTypeChecks (MTPrim mempty MTInt) (MyLiteral mempty (MyInt 100))
          `shouldBe` Right True
      it "typechecking fail works" $ do
        itTypeChecks (MTPrim mempty MTBool) (MyLiteral mempty (MyInt 100))
          `shouldBe` Right False
    describe "Test generators" $ do
      it "Bool" $ do
        itGenerates mtBool
      it "Int" $ do
        itGenerates mtInt
      it "String" $ do
        itGenerates mtString
