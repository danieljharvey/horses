{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Exhaustiveness
  ( spec,
  )
where

import qualified Data.Map as M
import Language.Mimsa.Typechecker.Exhaustiveness
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Test.Hspec

exhaustiveCheck :: [Pattern Name ()] -> [Pattern Name ()]
exhaustiveCheck = isExhaustive

spec :: Spec
spec = do
  describe "Exhaustiveness" $ do
    it "Wildcard is fine" $ do
      exhaustiveCheck [PWildcard mempty] `shouldBe` []
    it "Var alone is fine" $ do
      exhaustiveCheck [PVar mempty "a"] `shouldBe` []
    it "Both True and False is fine" $ do
      exhaustiveCheck
        [ PLit mempty (MyBool False),
          PLit
            mempty
            ( MyBool True
            )
        ]
        `shouldBe` []
    it "Just True return Lit False" $ do
      exhaustiveCheck
        [PLit mempty (MyBool False)]
        `shouldBe` [ PLit
                       mempty
                       ( MyBool True
                       )
                   ]
    it "Just False return Lit True" $ do
      exhaustiveCheck
        [PLit mempty (MyBool True)]
        `shouldBe` [ PLit
                       mempty
                       ( MyBool False
                       )
                   ]
    it "Int literal returns Wildcard" $ do
      exhaustiveCheck [PLit mempty (MyInt 1)] `shouldBe` [PWildcard mempty]
    it "String literal returns Wildcard" $ do
      exhaustiveCheck [PLit mempty (MyString "hello")] `shouldBe` [PWildcard mempty]
    it "Int then var is exhaustive" $ do
      exhaustiveCheck
        [ PLit mempty (MyInt 1),
          PVar mempty "otherwise"
        ]
        `shouldBe` mempty

    it "Pair of vars is fine" $ do
      exhaustiveCheck
        [ PPair
            mempty
            (PWildcard mempty)
            (PWildcard mempty)
        ]
        `shouldBe` []
    it "Pair of False is returned" $
      do
        let true = PLit mempty (MyBool True)
            false = PLit mempty (MyBool False)
        exhaustiveCheck
          [ PPair mempty true true,
            PPair mempty false true,
            PPair mempty true false
          ]
          `shouldBe` [PPair mempty false false]
    it "Pair with var is exhaustive" $ do
      let true = PLit mempty (MyBool True)
          false = PLit mempty (MyBool False)
      exhaustiveCheck
        [ PPair mempty true true,
          PPair mempty false true,
          PPair mempty (PVar mempty "dog") false
        ]
        `shouldBe` []

    it "Record with two False values is returned" $ do
      let true = PLit mempty (MyBool True)
          false = PLit mempty (MyBool False)
      exhaustiveCheck
        [ PRecord mempty (M.fromList [("a", true), ("b", true)]),
          PRecord mempty (M.fromList [("a", false), ("b", true)]),
          PRecord mempty (M.fromList [("a", true), ("b", false)])
        ]
        `shouldBe` [PRecord mempty (M.fromList [("a", false), ("b", false)])]
    it "Constructor returns unused constructor" $ do
      exhaustiveCheck
        [PConstructor mempty "Just" [PWildcard mempty]]
        `shouldBe` [PConstructor mempty "Nothing" []]
