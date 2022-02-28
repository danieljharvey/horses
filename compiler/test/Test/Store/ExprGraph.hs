{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Store.ExprGraph
  ( spec,
  )
where

import Language.Mimsa.Store.ExprGraph
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Graphviz
import Language.Mimsa.Types.Identifiers
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec =
  describe "ExprGraph" $ do
    describe "numberExpr" $ do
      it "Numbers a literal" $ do
        numberExpr (unsafeParseExpr "True")
          `shouldBe` MyLiteral ((), 0) (MyBool True)
      it "Numbers an if" $ do
        numberExpr @Name @()
          ( MyIf
              ()
              (MyLiteral () (MyBool True))
              (MyLiteral () (MyInt 1))
              (MyLiteral () (MyInt 2))
          )
          `shouldBe` MyIf
            ((), 0)
            (MyLiteral ((), 1) (MyBool True))
            (MyLiteral ((), 2) (MyInt 1))
            (MyLiteral ((), 3) (MyInt 2))

      it "Numbers a let" $ do
        numberExpr (unsafeParseExpr "let a = 1 in a")
          `shouldBe` MyLet
            ((), 0)
            (Identifier ((), 1) "a")
            (MyLiteral ((), 2) (MyInt 1))
            (MyVar ((), 3) "a")
      it "Numbers a lambda" $ do
        numberExpr (unsafeParseExpr "\\a -> if True then 1 else 2")
          `shouldBe` MyLambda
            ((), 0)
            (Identifier ((), 1) "a")
            ( MyIf
                ((), 2)
                (MyLiteral ((), 3) (MyBool True))
                (MyLiteral ((), 4) (MyInt 1))
                (MyLiteral ((), 5) (MyInt 2))
            )

      it "Numbers a match" $ do
        numberExpr (unsafeParseExpr "\\a -> match True with _ -> True")
          `shouldBe` MyLambda
            ((), 0)
            (Identifier ((), 1) "a")
            ( MyPatternMatch
                ((), 2)
                (MyLiteral ((), 3) (MyBool True))
                [(PWildcard ((), 4), MyLiteral ((), 5) (MyBool True))]
            )
    describe "createExprGraph" $ do
      it "Graphs a literal" $ do
        let graph = createExprGraph (unsafeParseExpr "True")
        prettyGraphviz graph `shouldBe` ""
      it "Graphs an if" $ do
        let graph =
              createExprGraph (unsafeParseExpr "if True then 1 else 2")
        prettyGraphviz graph
          `shouldBe` ""
      it "Graphs a lambda" $ do
        let graph =
              createExprGraph (unsafeParseExpr "\\a -> True")
        prettyGraphviz graph
          `shouldBe` ""
      it "Graphs a pattern match" $ do
        let graph =
              createExprGraph
                ( unsafeParseExpr
                    "\\a -> match a with (Just b) -> 100 | Nothing -> 0"
                )
        prettyGraphviz graph
          `shouldBe` ""
