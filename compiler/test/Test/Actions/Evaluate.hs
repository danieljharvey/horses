{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.Evaluate
  ( spec,
  )
where

import Data.Either (isLeft)
import Data.Functor
import qualified Language.Mimsa.Actions.Evaluate as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

brokenExpr :: Expr Name Annotation
brokenExpr = MyInfix mempty Equals (int 1) (bool True)

onePlusOneExpr :: Expr Name Annotation
onePlusOneExpr = MyInfix mempty Add (int 1) (int 1)

spec :: Spec
spec = do
  describe "Evaluate" $ do
    it "Should return an error for a broken expr" $ do
      Actions.run testStdlib (Actions.evaluate (prettyPrint brokenExpr) brokenExpr) `shouldSatisfy` isLeft
    it "Should evaluate an expression" $ do
      let action = Actions.evaluate (prettyPrint onePlusOneExpr) onePlusOneExpr
      let (newProject, _, (mt, expr, _, _, _)) = fromRight (Actions.run testStdlib action)
      mt $> () `shouldBe` MTPrim mempty MTInt
      expr $> () `shouldBe` int 2
      -- project should be untouched
      newProject `shouldBe` testStdlib
