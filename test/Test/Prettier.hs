{-# LANGUAGE OverloadedStrings #-}

module Test.Prettier
  ( spec,
  )
where

import qualified Data.Map as M
import qualified Data.Text.IO as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec =
  describe "Prettier" $ do
    describe "Expr"
      $ it "Cons with infix"
      $ do
        let expr' :: Expr Name ()
            expr' =
              MyConsApp
                ()
                (MyConstructor mempty "Some")
                (MyInfix mempty Equals (int 1) (int 1))
        prettyPrint expr'
          `shouldBe` "Some (1 == 1)"
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
