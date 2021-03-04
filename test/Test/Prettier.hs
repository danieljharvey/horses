{-# LANGUAGE OverloadedStrings #-}

module Test.Prettier
  ( spec,
  )
where

import Data.Functor (($>))
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as T
import Language.Mimsa.Parser
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers

unsafeParseExpr :: Text -> Expr Name ()
unsafeParseExpr t = case parseExpr t of
  Right a -> a $> ()
  Left _ -> error "Error parsing expr for Prettier tests"

spec :: Spec
spec =
  describe "Prettier" $ do
    describe "Expr" $ do
      it "Cons with infix" $
        do
          let expr' :: Expr Name ()
              expr' =
                MyConsApp
                  ()
                  (MyConstructor mempty "Some")
                  (MyInfix mempty Equals (int 1) (int 1))
          prettyPrint expr'
            `shouldBe` "Some (1 == 1)"
      it "Many + operators" $ do
        let expr' :: Expr Name ()
            expr' = unsafeParseExpr "1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10"
        let doc = prettyDoc expr'
        renderWithWidth 50 doc `shouldBe` "1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10"
        renderWithWidth 5 doc `shouldBe` "1 + 2\n  + 3\n  + 4\n  + 5\n  + 6\n  + 7\n  + 8\n  + 9\n  + 10"
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
                    [ ("dog", MTPrim mempty MTUnit),
                      ("horse", MTPrim mempty MTString),
                      ( "maybeDog",
                        MTData
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
