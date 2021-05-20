{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Exhaustiveness
  ( spec,
  )
where

import qualified Data.Map as M
import Language.Mimsa.Typechecker.Exhaustiveness
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Typechecker.Codegen.Shared

exhaustiveCheck ::
  [Pattern Name Annotation] ->
  Either TypeError [Pattern Name Annotation]
exhaustiveCheck = runPatternM testEnv . isExhaustive
  where
    testEnv = Environment mempty dts mempty
    dts = M.fromList [("Maybe", dtMaybe), ("These", dtThese)]

spec :: Spec
spec = do
  describe "Exhaustiveness" $ do
    it "Wildcard is fine" $ do
      exhaustiveCheck [PWildcard mempty] `shouldBe` Right []
    it "Var alone is fine" $ do
      exhaustiveCheck [PVar mempty "a"] `shouldBe` Right []
    it "Both True and False is fine" $ do
      exhaustiveCheck
        [ PLit mempty (MyBool False),
          PLit
            mempty
            ( MyBool True
            )
        ]
        `shouldBe` Right []
    it "Just True return Lit False" $ do
      exhaustiveCheck
        [PLit mempty (MyBool False)]
        `shouldBe` Right
          [ PLit
              mempty
              ( MyBool True
              )
          ]
    it "Just False return Lit True" $ do
      exhaustiveCheck
        [PLit mempty (MyBool True)]
        `shouldBe` Right
          [ PLit
              mempty
              ( MyBool False
              )
          ]
    it "Int literal returns Wildcard" $ do
      exhaustiveCheck [PLit mempty (MyInt 1)]
        `shouldBe` Right [PWildcard mempty]
    it "String literal returns Wildcard" $ do
      exhaustiveCheck [PLit mempty (MyString "hello")]
        `shouldBe` Right [PWildcard mempty]
    it "Int then var is exhaustive" $ do
      exhaustiveCheck
        [ PLit mempty (MyInt 1),
          PVar mempty "otherwise"
        ]
        `shouldBe` Right mempty

    it "Pair of vars is fine" $ do
      exhaustiveCheck
        [ PPair
            mempty
            (PWildcard mempty)
            (PWildcard mempty)
        ]
        `shouldBe` Right []
    it "Pair of False is returned" $
      do
        let true = PLit mempty (MyBool True)
            false = PLit mempty (MyBool False)
        exhaustiveCheck
          [ PPair mempty true true,
            PPair mempty false true,
            PPair mempty true false
          ]
          `shouldBe` Right [PPair mempty false false]
    it "Pair with var is exhaustive" $ do
      let true = PLit mempty (MyBool True)
          false = PLit mempty (MyBool False)
      exhaustiveCheck
        [ PPair mempty true true,
          PPair mempty false true,
          PPair mempty (PVar mempty "dog") false
        ]
        `shouldBe` Right []

    it "Record with two False values is returned" $ do
      let true = PLit mempty (MyBool True)
          false = PLit mempty (MyBool False)
      exhaustiveCheck
        [ PRecord mempty (M.fromList [("a", true), ("b", true)]),
          PRecord mempty (M.fromList [("a", false), ("b", true)]),
          PRecord mempty (M.fromList [("a", true), ("b", false)])
        ]
        `shouldBe` Right [PRecord mempty (M.fromList [("a", false), ("b", false)])]
    it "Constructor returns unused constructor" $ do
      exhaustiveCheck
        [PConstructor mempty "Just" [PWildcard mempty]]
        `shouldBe` Right [PConstructor mempty "Nothing" []]
    it "Constructor returns unused items inside it" $ do
      exhaustiveCheck
        [ PConstructor mempty "Just" [PLit mempty (MyBool True)],
          PConstructor mempty "Nothing" mempty
        ]
        `shouldBe` Right
          [ PConstructor
              mempty
              "Just"
              [ PLit
                  mempty
                  (MyBool False)
              ]
          ]
    it "Constructor returns multiple unused constructors" $ do
      exhaustiveCheck
        [ PConstructor mempty "This" [PWildcard mempty]
        ]
        `shouldBe` Right
          [ PConstructor mempty "That" [PWildcard mempty],
            PConstructor mempty "These" [PWildcard mempty, PWildcard mempty]
          ]
    it "Nested constructors" $ do
      exhaustiveCheck
        [ PConstructor mempty "Just" [PConstructor mempty "Nothing" mempty],
          PConstructor mempty "Just" [PWildcard mempty]
        ]
        `shouldBe` Right
          [ PConstructor mempty "Just" [PConstructor mempty "Just" [PWildcard mempty]],
            PConstructor mempty "Nothing" []
          ]
