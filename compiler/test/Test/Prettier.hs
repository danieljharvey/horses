{-# LANGUAGE OverloadedStrings #-}

module Test.Prettier
  ( spec,
  )
where

import qualified Data.Map as M
import qualified Data.Text.IO as T
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec =
  describe "Prettier" $ do
    describe "Expr" $ do
      it "Cons with infix" $ do
        let expr' = unsafeParseExpr "Some (1 == 1)"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "Some (1 == 1)"

      it "Many + operators" $ do
        let expr' = unsafeParseExpr "1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10"
        renderWithWidth 5 doc `shouldBe` "1 + 2\n  + 3\n  + 4\n  + 5\n  + 6\n  + 7\n  + 8\n  + 9\n  + 10"

      it "Nested lambdas" $ do
        let expr' = unsafeParseExpr "\\f -> \\g -> \\a -> f (g a)"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "\\f -> \\g -> \\a -> f (g a)"
        renderWithWidth 5 doc `shouldBe` "\\f ->\n  \\g ->\n    \\a ->\n      f (g a)"

      it "Line between let bindings" $ do
        let expr' = unsafeParseExpr "let a = 1; a"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "let a = 1 in a"
        renderWithWidth 5 doc `shouldBe` "let a =\n  1;\n\na"

      it "Line between let pair bindings" $ do
        let expr' = unsafeParseExpr "let (a,b) = ((1,2)); a"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "let (a, b) = ((1, 2)) in a"
        renderWithWidth 5 doc `shouldBe` "let (a, b) =\n  ((1,\n    2));\n\na"

      it "Line between infix bindings" $ do
        let expr' = unsafeParseExpr "infix >> = compose in a"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "infix >> = compose in a"
        renderWithWidth 5 doc `shouldBe` "infix >> = compose;\n\na"

      it "Spreads long pairs across two lines" $ do
        let expr' = unsafeParseExpr "(\"horseshorseshorses1\",\"horseshorseshorses2\")"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "(\"horseshorseshorses1\", \"horseshorseshorses2\")"
        renderWithWidth 5 doc `shouldBe` "(\"horseshorseshorses1\",\n \"horseshorseshorses2\")"

      it "Renders empty record nicely" $ do
        let expr' = unsafeParseExpr "{}"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "{}"
        renderWithWidth 5 doc `shouldBe` "{}"

      it "Renders records nicely" $ do
        let expr' = unsafeParseExpr "{a:1,b:2,c:3,d:4,e:5}"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "{ a: 1, b: 2, c: 3, d: 4, e: 5 }"
        renderWithWidth 5 doc `shouldBe` "{ a: 1,\n  b: 2,\n  c: 3,\n  d: 4,\n  e: 5 }"

      it "Renders if nicely" $ do
        let expr' = unsafeParseExpr "if True then 1 else 2"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "if True then 1 else 2"
        renderWithWidth 4 doc `shouldBe` "if True\nthen\n  1\nelse\n  2"

      it "Renders datatype nicely with two line break" $ do
        let expr' = unsafeParseExpr "type These a = That a in 1"
            doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "type These a    = That a in 1"
        renderWithWidth 5 doc `shouldBe` "type These a \n  = That\n  a;\n\n1"

    describe
      "MonoType"
      $ do
        it "String" $
          T.putStrLn (prettyPrint MTString)
        it "Function" $
          let mt :: MonoType
              mt =
                MTFunction
                  mempty
                  (MTFunction mempty (MTPrim mempty MTInt) (MTPrim mempty MTString))
                  (MTPrim mempty MTBool)
           in T.putStrLn
                ( prettyPrint mt
                )
        it "Record" $
          let mt :: MonoType
              mt =
                MTRecord mempty $
                  M.fromList
                    [ ("dog", MTPrim mempty MTBool),
                      ("horse", MTPrim mempty MTString),
                      ( "maybeDog",
                        dataTypeWithVars
                          mempty
                          "Maybe"
                          [MTPrim mempty MTString]
                      )
                    ]
           in T.putStrLn
                ( prettyPrint mt
                )
        it "Pair" $
          let mt :: MonoType
              mt =
                MTPair
                  mempty
                  (MTFunction mempty (MTPrim mempty MTInt) (MTPrim mempty MTInt))
                  (MTPrim mempty MTString)
           in T.putStrLn
                (prettyPrint mt)
        it "Variables" $
          let mt :: MonoType
              mt =
                MTFunction
                  mempty
                  ( MTVar mempty $
                      tvNamed "catch"
                  )
                  (MTVar mempty $ tvNumbered 22)
           in T.putStrLn
                ( prettyPrint mt
                )
        it "Names type vars" $ do
          let mt = MTVar () (tvNumbered 1)
          prettyPrint mt `shouldBe` "a"
        it "Names type vars 2" $ do
          let mt = MTVar () (tvNumbered 26)
          prettyPrint mt `shouldBe` "z"
        it "Names type vars 3" $ do
          let mt = MTVar () (tvNumbered 27)
          prettyPrint mt `shouldBe` "a1"
