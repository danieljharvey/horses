{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.AnnotateExpression
  ( spec,
  )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.AnnotateExpression
import Language.Mimsa.Typechecker.Infer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Typechecker
import Test.Hspec

getTypesList' :: Text -> Either Text [TypedVariable]
getTypesList' input = do
  (ResolvedExpression _ _ expr _ swaps _) <-
    first prettyPrint (Actions.evaluateText mempty input)
  (subs, _mt) <-
    first
      prettyPrint
      (inferAndSubst mempty mempty mempty expr)
  pure (getTypesList swaps subs)

spec :: Spec
spec = do
  describe "AnnotateExpression" $ do
    describe "getTypesList" $ do
      it "Finds type of variable" $ do
        getTypesList' "let a = 123 in True"
          `shouldBe` Right [("a", MTPrim mempty MTInt, Location 0 19)]
      it "Finds type of variable 2" $ do
        getTypesList' "let a = 123; let b = False in True"
          `shouldBe` Right
            [ ("a", MTPrim mempty MTInt, Location 0 34),
              ("b", MTPrim mempty MTBool, Location 13 34)
            ]
      it "Finds type of shadowed variables" $ do
        getTypesList' "let a = 123; let a = False in True"
          `shouldBe` Right
            [ ("a", MTPrim mempty MTInt, Location 0 34),
              ("a", MTPrim mempty MTBool, Location 13 34)
            ]
      it "Finds type of variable introduced by lambda" $ do
        getTypesList' "let good = \\a -> True; good(1)"
          `shouldBe` Right
            [ ("good", MTFunction mempty (MTPrim mempty MTInt) (MTPrim mempty MTBool), Location 0 30),
              ("a", MTPrim mempty MTInt, Location 11 21)
            ]
