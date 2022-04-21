{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Exhaustiveness
  ( spec,
  )
where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Either
import qualified Data.Map as M
import Language.Mimsa.Typechecker.Exhaustiveness
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker
import Test.Codegen.Shared
import Test.Hspec

type PatternM = ReaderT Swaps (ExceptT TypeError Identity)

runPatternM ::
  PatternM a ->
  Either TypeError a
runPatternM value =
  runIdentity (runExceptT (runReaderT value mempty))

exhaustiveCheck ::
  [Pattern Name Annotation] ->
  Either TypeError [Pattern Name Annotation]
exhaustiveCheck = runPatternM . isExhaustive testEnv

redundantCasesCheck ::
  [Pattern Name Annotation] ->
  Either TypeError [Pattern Name Annotation]
redundantCasesCheck = runPatternM . redundantCases testEnv

noDuplicatesCheck ::
  Pattern Name Annotation ->
  Either TypeError ()
noDuplicatesCheck = runPatternM . noDuplicateVariables

testEnv :: Environment
testEnv = Environment mempty dts mempty mempty
  where
    dts = M.fromList [("Maybe", dtMaybe), ("These", dtThese)]

spec :: Spec
spec = do
  describe "Smaller list versions" $ do
    it "Empty is empty" $ do
      smallerListVersions [] `shouldBe` ([] :: [[Int]])
    it "1 list is the same" $ do
      smallerListVersions [[1]] `shouldBe` ([[1]] :: [[Int]])
    it "2 list adds a 1 list" $ do
      smallerListVersions [[1, 2]] `shouldBe` ([[2], [1, 2]] :: [[Int]])
    it "3 list adds a 2 and a 1 list" $ do
      smallerListVersions [[1, 2, 3]] `shouldBe` ([[3], [2, 3], [1, 2, 3]] :: [[Int]])

  describe "Exhaustiveness" $
    do
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
      it "A pair annihilates empty" $ do
        exhaustiveCheck
          [ PConstructor mempty "Just" [PPair mempty (PWildcard mempty) (PWildcard mempty)],
            PConstructor mempty "Nothing" mempty
          ]
          `shouldBe` Right mempty
      it "A record annihilates empty" $ do
        exhaustiveCheck
          [ PConstructor mempty "Just" [PRecord mempty mempty],
            PConstructor mempty "Nothing" mempty
          ]
          `shouldBe` Right mempty
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
                ],
              PConstructor
                mempty
                "Just"
                [ PWildcard mempty
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
            [ PConstructor mempty "Nothing" []
            ]
      it "A var is equivalent to a wildcard" $ do
        exhaustiveCheck
          [ PConstructor mempty "Just" [PVar mempty "a"],
            PConstructor mempty "Nothing" mempty
          ]
          `shouldBe` Right []
      it "Multiple int literals" $ do
        exhaustiveCheck [PLit mempty (MyInt 1), PLit mempty (MyInt 2)]
          `shouldBe` Right [PWildcard mempty]
      it "NoSpread Array produces wildcard" $ do
        exhaustiveCheck [PArray mempty [PLit mempty (MyInt 1)] NoSpread]
          `shouldBe` Right
            [ PArray mempty [PWildcard mempty] (SpreadWildcard mempty), -- one or more
              PArray mempty [] NoSpread -- empty
            ]
      it "Spread Array produces all other sized spread cases" $ do
        exhaustiveCheck [PArray mempty [PLit mempty (MyInt 1)] (SpreadValue mempty "a")]
          `shouldBe` Right
            [ PArray mempty [PWildcard mempty] (SpreadWildcard mempty), -- one or more
              PArray mempty [] NoSpread -- empty
            ]
      it "Larger Spread Array produces all other sized spread cases" $ do
        exhaustiveCheck [PArray mempty [PLit mempty (MyInt 1), PLit mempty (MyInt 2)] (SpreadValue mempty "a")]
          `shouldBe` Right
            [ PArray mempty [PWildcard mempty] (SpreadWildcard mempty), -- one or more
              PArray mempty [PWildcard mempty, PWildcard mempty] (SpreadWildcard mempty), -- two or more
              PArray mempty [] NoSpread -- empty
            ]
      it "NoSpread empty array produces wildcard" $ do
        exhaustiveCheck [PArray mempty mempty NoSpread]
          `shouldBe` Right [PArray mempty [PWildcard mempty] (SpreadWildcard mempty)]

      it "A string match produces empty string" $ do
        exhaustiveCheck [PString mempty (StrWildcard mempty) (StrWildcard mempty)]
          `shouldBe` Right [PLit mempty (MyString "")]
  describe "Redundant cases" $ do
    it "Returns none" $ do
      redundantCasesCheck [PWildcard mempty] `shouldBe` Right mempty
    it "Returns anything after a wildcard (1)" $ do
      redundantCasesCheck
        [ PWildcard mempty,
          PLit mempty (MyBool True)
        ]
        `shouldBe` Right
          [ PLit mempty (MyBool True)
          ]
    it "Returns anything after a wildcard (2)" $ do
      redundantCasesCheck
        [ PWildcard mempty,
          PLit mempty (MyBool True),
          PLit mempty (MyBool False)
        ]
        `shouldBe` Right
          [ PLit mempty (MyBool True),
            PLit mempty (MyBool False)
          ]
    it "Works with constructors" $ do
      redundantCasesCheck
        [ PConstructor mempty "Just" [PWildcard mempty],
          PConstructor mempty "Just" [PLit mempty (MyInt 1)],
          PConstructor mempty "Nothing" mempty
        ]
        `shouldBe` Right
          [ PConstructor
              mempty
              "Just"
              [PLit mempty (MyInt 1)]
          ]
    it "Multiple ints make wildcard necessary" $ do
      redundantCasesCheck
        [PLit mempty (MyInt 1), PLit mempty (MyInt 2), PWildcard mempty]
        `shouldBe` Right []
  describe "noDuplicateVariables" $ do
    it "Is fine with wildcard" $ do
      noDuplicatesCheck (PWildcard mempty) `shouldSatisfy` isRight
    it "Is fine with lit" $ do
      noDuplicatesCheck (PLit mempty (MyBool True)) `shouldSatisfy` isRight
    it "Is fine with single var" $ do
      noDuplicatesCheck (PVar mempty ("a")) `shouldSatisfy` isRight
    it "Is fine with a pair of different vars" $ do
      noDuplicatesCheck
        ( PPair
            mempty
            (PVar mempty ("a"))
            (PVar mempty ("b"))
        )
        `shouldSatisfy` isRight
    it "Hates a pair of the same var" $ do
      noDuplicatesCheck
        ( PPair
            mempty
            (PVar mempty ("a"))
            (PVar mempty ("a"))
        )
        `shouldSatisfy` isLeft
    it "Is fine with a record of uniques" $ do
      noDuplicatesCheck
        ( PRecord
            mempty
            ( M.fromList
                [ ("dog", PVar mempty ("a")),
                  ("log", PVar mempty ("b"))
                ]
            )
        )
        `shouldSatisfy` isRight
    it "Is not fine with a record with dupes" $ do
      noDuplicatesCheck
        ( PRecord
            mempty
            ( M.fromList
                [ ("dog", PVar mempty ("a")),
                  ("log", PVar mempty ("a"))
                ]
            )
        )
        `shouldSatisfy` isLeft
    it "Is fine with an array with no dupes" $ do
      noDuplicatesCheck
        ( PArray
            mempty
            [ PVar mempty ("a"),
              PVar mempty ("b")
            ]
            NoSpread
        )
        `shouldSatisfy` isRight
    it "Is not fine with an array with dupes" $ do
      noDuplicatesCheck
        ( PArray
            mempty
            [ PVar mempty ("a"),
              PVar mempty ("a")
            ]
            NoSpread
        )
        `shouldSatisfy` isLeft
    it "Is not fine with an array with dupes with the spread" $ do
      noDuplicatesCheck
        ( PArray
            mempty
            [ PVar mempty ("a"),
              PVar mempty ("b")
            ]
            (SpreadValue mempty ("a"))
        )
        `shouldSatisfy` isLeft
    it "Is fine with a constructor with no dupes" $ do
      noDuplicatesCheck
        ( PConstructor
            mempty
            "Dog"
            [ PVar mempty ("a"),
              PVar mempty ("b")
            ]
        )
        `shouldSatisfy` isRight
    it "Is not fine with a constructor with dupes" $ do
      noDuplicatesCheck
        ( PConstructor
            mempty
            "Dog"
            [ PVar mempty ("a"),
              PVar mempty ("b"),
              PConstructor
                mempty
                "Dog"
                [ PVar mempty ("c"),
                  PVar mempty ("a")
                ]
            ]
        )
        `shouldSatisfy` isLeft
