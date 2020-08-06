{-# LANGUAGE OverloadedStrings #-}

module Test.Library
  ( spec,
  )
where

import qualified Data.List.NonEmpty as NE
import Language.Mimsa.Library (reduce)
import Language.Mimsa.Types
import Test.Helpers
import Test.Hspec

spec :: Spec
spec =
  describe "Library" $ do
    it "reduce with 1 item" $ do
      let f = MyLambda (named "b") (MyLambda (named "a") (MyVar (named "b")))
          b = int 0
          as = NE.fromList [int 1]
          result = reduce f b as
          expected =
            MyLet
              (named "f")
              f
              ( MyApp
                  ( MyApp
                      (MyVar (named "f"))
                      (MyLiteral (MyInt 0))
                  )
                  (MyLiteral (MyInt 1))
              )
      result `shouldBe` expected
    it "reduce with 2 items" $ do
      let f = MyLambda (named "b") (MyLambda (named "a") (MyVar (named "b")))
          b = int 0
          as = NE.fromList [int 1, int 2]
          result = reduce f b as
          expected =
            MyLet
              (named "f")
              f
              ( MyApp
                  ( MyApp
                      (MyVar (named "f"))
                      ( MyApp
                          ( MyApp
                              (MyVar (named "f"))
                              (int 0)
                          )
                          (int 1)
                      )
                  )
                  (int 2)
              )
      result `shouldBe` expected
