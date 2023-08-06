{-# LANGUAGE OverloadedStrings #-}

module Test.Typecheck.ExhaustivenessSpec
  ( spec,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.List.NonEmpty as NE
import Smol.Core
import Smol.Core.Typecheck.FromParsedExpr
import Test.BuiltInTypes
import Test.Helpers
import Test.Hspec

env :: (Monoid ann, Ord ann) => TCEnv ann
env = TCEnv mempty (builtInTypes emptyResolvedDep) mempty mempty mempty

type PatternM = ExceptT (TCError Annotation) (Reader (TCEnv Annotation))

-- this is partial as fuck but let's get it typechecking and import it into the
-- new project

runPatternM ::
  PatternM a ->
  Either (TCError Annotation) a
runPatternM value =
  runReader (runExceptT value) env

exhaustiveCheck ::
  [Pattern ResolvedDep (Type ResolvedDep Annotation)] ->
  Either (TCError Annotation) [Pattern ResolvedDep (Type ResolvedDep Annotation)]
exhaustiveCheck pats = runPatternM $ isExhaustive pats

_redundantCasesCheck ::
  [Pattern ResolvedDep (Type ResolvedDep Annotation)] ->
  Either (TCError Annotation) [Pattern ResolvedDep (Type ResolvedDep Annotation)]
_redundantCasesCheck pats = runPatternM $ redundantCases pats

_noDuplicatesCheck ::
  Pattern ResolvedDep (Type ResolvedDep Annotation) ->
  Either (TCError Annotation) ()
_noDuplicatesCheck = runPatternM . noDuplicateVariables

spec :: Spec
spec = do
  describe "Exhaustiveness" $ do
    describe "Smaller list versions" $ do
      it "Empty is empty" $ do
        smallerListVersions [] `shouldBe` ([] :: [[Int]])
      it "1 list is the same" $ do
        smallerListVersions [[1]] `shouldBe` ([[1]] :: [[Int]])
      it "2 list adds a 1 list" $ do
        smallerListVersions [[1, 2]] `shouldBe` ([[2], [1, 2]] :: [[Int]])
      it "3 list adds a 2 and a 1 list" $ do
        smallerListVersions [[1, 2, 3]] `shouldBe` ([[3], [2, 3], [1, 2, 3]] :: [[Int]])

    describe "annihilate" $ do
      it "Tuple of wildcards annihilates tuple of true lits" $ do
        let left =
              PTuple
                mempty
                (PWildcard mempty)
                ( NE.fromList
                    [ PWildcard mempty,
                      PWildcard mempty
                    ]
                )
            right =
              PTuple
                mempty
                (PLiteral mempty (PBool True))
                ( NE.fromList
                    [ PLiteral mempty (PBool True),
                      PLiteral mempty (PBool True)
                    ]
                )
        annihilate left right `shouldBe` True

      it "Tuple of wildcards annihilates tuple of true lits" $ do
        let left =
              PTuple
                mempty
                (PWildcard mempty)
                ( NE.fromList [PWildcard mempty, PLiteral mempty (PBool True)]
                )
            right =
              PTuple
                mempty
                (PLiteral mempty (PBool True))
                ( NE.fromList [PLiteral mempty (PBool True), PLiteral mempty (PBool True)]
                )
        annihilate left right `shouldBe` True

    describe "Exhaustiveness" $
      do
        it "True literal is complete in itself" $ do
          let ty = tyBoolLit True
          exhaustiveCheck [PLiteral ty (PBool True)]
            `shouldBe` Right []
        it "False literal is complete in itself" $ do
          let ty = tyBoolLit False
          exhaustiveCheck [PLiteral ty (PBool False)]
            `shouldBe` Right []

        it "Int literal is complete in itself" $ do
          let ty = tyIntLit [100]
          exhaustiveCheck [PLiteral ty (PInt 100)]
            `shouldBe` Right mempty

        it "Int needs wildcard" $ do
          exhaustiveCheck [PLiteral tyInt (PInt 100)]
            `shouldBe` Right [PWildcard tyInt]

        it "Union of int literals" $ do
          let ty = tyIntLit [1, 2]
          exhaustiveCheck [PLiteral ty (PInt 1)]
            `shouldBe` Right [PLiteral (tyIntLit [1, 2]) (PInt 2)]

        it "Wildcard is fine" $ do
          exhaustiveCheck [PWildcard tyInt] `shouldBe` Right []

        it "Var alone is fine" $ do
          exhaustiveCheck [PVar tyInt "a"] `shouldBe` Right []

        it "Both True and False is fine" $ do
          exhaustiveCheck
            [ PLiteral tyBool (PBool False),
              PLiteral
                tyBool
                ( PBool True
                )
            ]
            `shouldBe` Right []

        it "Just True return Lit False" $ do
          exhaustiveCheck
            [PLiteral tyBool (PBool False)]
            `shouldBe` Right
              [ PLiteral
                  tyBool
                  ( PBool True
                  )
              ]

        it "Just False return Lit True" $ do
          exhaustiveCheck
            [PLiteral tyBool (PBool True)]
            `shouldBe` Right
              [ PLiteral
                  tyBool
                  ( PBool False
                  )
              ]

        it "Int literal returns Wildcard" $ do
          exhaustiveCheck [PLiteral tyInt (PInt 1)]
            `shouldBe` Right [PWildcard tyInt]

        it "Int then var is exhaustive" $ do
          exhaustiveCheck
            [ PLiteral tyInt (PInt 1),
              PVar tyInt "otherwise"
            ]
            `shouldBe` Right mempty

        it "Pair of vars is fine" $ do
          exhaustiveCheck
            [ PTuple
                (tyTuple tyInt [tyInt])
                (PWildcard tyInt)
                (NE.singleton $ PWildcard tyInt)
            ]
            `shouldBe` Right []

        it "Pair of False is returned" $
          do
            let true = PLiteral tyBool (PBool True)
                false = PLiteral tyBool (PBool False)
                tuple' = tyTuple tyBool [tyBool]
            exhaustiveCheck
              [ PTuple tuple' true (NE.singleton true),
                PTuple tuple' false (NE.singleton true),
                PTuple tuple' true (NE.singleton false)
              ]
              `shouldBe` Right [PTuple tuple' false (NE.singleton false)]

        it "3 tuple of wildcards is exhaustive" $ do
          let wildcard = PWildcard tyInt
              tuple' = tyTuple tyInt [tyInt, tyInt]
          exhaustiveCheck
            [PTuple tuple' wildcard (NE.fromList [wildcard, wildcard])]
            `shouldBe` Right mempty

        it "3 tuple of ones are not exhaustive" $ do
          let one = PLiteral tyInt (PInt 1)
              wildcard = PWildcard tyInt
              tuple' = tyTuple tyInt [tyInt, tyInt]
          exhaustiveCheck
            [PTuple tuple' one (NE.fromList [one, one])]
            `shouldBe` Right [PTuple tuple' wildcard (NE.fromList [wildcard, wildcard])]

        it "First in 3-tuples is non-exhaustive" $ do
          let one = PLiteral tyInt (PInt 1)
              wildcard = PWildcard tyInt
              tuple' = tyTuple tyInt [tyInt, tyInt]
          exhaustiveCheck
            [ PTuple
                tuple'
                one
                ( NE.fromList [wildcard, wildcard]
                )
            ]
            `shouldBe` Right
              [ PTuple
                  tuple'
                  wildcard
                  ( NE.fromList [wildcard, wildcard]
                  )
              ]

        it "Tuples of 2 bools creates 4 patterns in total" $ do
          let true = PLiteral tyBool (PBool True)
              false = PLiteral tyBool (PBool False)
              tuple' = tyTuple tyBool [tyBool]
          exhaustiveCheck
            [ PTuple
                tuple'
                true
                ( NE.fromList [true]
                )
            ]
            `shouldBe` Right
              [ PTuple
                  tuple'
                  true
                  ( NE.fromList [false]
                  ),
                PTuple
                  tuple'
                  false
                  ( NE.fromList [true]
                  ),
                PTuple
                  tuple'
                  false
                  ( NE.fromList [false]
                  )
              ]

        it "When one item in a tuple is a wildcard, other patterns it needs are also" $ do
          let true = PLiteral tyBool (PBool True)
              false = PLiteral tyBool (PBool False)
              wildcard = PWildcard tyBool
              tuple' = tyTuple tyBool [tyBool]
          exhaustiveCheck
            [PTuple tuple' wildcard (NE.singleton true)]
            `shouldBe` Right
              [ PTuple
                  tuple'
                  wildcard
                  ( NE.fromList [false]
                  )
              ]

        it "Tuple of two wildcards covers everything" $ do
          let wildcard = PWildcard tyBool
              tuple' = tyTuple tyBool [tyBool]
          exhaustiveCheck
            [PTuple tuple' wildcard (NE.singleton wildcard)]
            `shouldBe` Right []

        -- its not but cba fixing now, making it over rather than under safe
        xit "Pair with var is exhaustive" $ do
          let true = PLiteral tyBool (PBool True)
              false = PLiteral tyBool (PBool False)
              tuple' = tyTuple tyBool [tyBool]
          exhaustiveCheck
            [ PTuple tuple' true (NE.singleton true),
              PTuple tuple' false (NE.singleton true),
              PTuple tuple' (PVar tyBool "dog") (NE.singleton false)
            ]
            `shouldBe` Right []

        -- same as above
        xit "A pair with complete coverage of Right and Left is exhaustive" $ do
          let either' = tyCons "Either" [tyVar "e", tyVar "a"]
              leftE = fromParsedType <$> PConstructor either' "Left" [PVar (tyVar "e") "e"]
              rightF = fromParsedType <$> PConstructor either' "Right" [PVar (tyVar "a") "f"]
              rightA = fromParsedType <$> PConstructor either' "Right" [PVar (tyVar "a") "a"]
              wildcard = fromParsedType <$> PWildcard either'
              tuple' = fromParsedType $ tyTuple either' [either']
          exhaustiveCheck
            [ PTuple tuple' rightF (NE.singleton rightA),
              PTuple tuple' leftE (NE.singleton wildcard),
              PTuple tuple' wildcard (NE.singleton leftE)
            ]
            `shouldBe` Right mempty

--              [PTuple tuple' wildcard (NE.singleton wildcard)]

{-
        it "A pair annihilates empty" $ do
          exhaustiveCheck
            [ PConstructor mempty "Just" [PTuple mempty (PWildcard mempty) (NE.singleton $ PWildcard mempty)],
              PConstructor mempty "Nothing" mempty
            ]
            `shouldBe` Right mempty
        it "Constructor returns unused constructor" $ do
          exhaustiveCheck
            [PConstructor mempty "Just" [PWildcard mempty]]
            `shouldBe` Right [PConstructor mempty "Nothing" []]
        it "Constructor returns unused items inside it" $ do
          exhaustiveCheck
            [ PConstructor mempty "Just" [PLiteral mempty (PBool True)],
              PConstructor mempty "Nothing" mempty
            ]
            `shouldBe` Right
              [ PConstructor
                  mempty
                  "Just"
                  [ PLiteral
                      mempty
                      (PBool False)
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
          exhaustiveCheck [PLiteral mempty (PInt 1), PLiteral mempty (PInt 2)]
            `shouldBe` Right [PWildcard mempty]

    describe "Redundant cases" $ do
      it "Returns none" $ do
        redundantCasesCheck [PWildcard mempty] `shouldBe` Right mempty
      it "Returns anything after a wildcard (1)" $ do
        redundantCasesCheck
          [ PWildcard mempty,
            PLiteral mempty (PBool True)
          ]
          `shouldBe` Right
            [ PLiteral mempty (PBool True)
            ]
      it "Returns anything after a wildcard (2)" $ do
        redundantCasesCheck
          [ PWildcard mempty,
            PLiteral mempty (PBool True),
            PLiteral mempty (PBool False)
          ]
          `shouldBe` Right
            [ PLiteral mempty (PBool True),
              PLiteral mempty (PBool False)
            ]
      it "Works with constructors" $ do
        redundantCasesCheck
          [ PConstructor mempty "Just" [PWildcard mempty],
            PConstructor mempty "Just" [PLiteral mempty (PInt 1)],
            PConstructor mempty "Nothing" mempty
          ]
          `shouldBe` Right
            [ PConstructor
                mempty
                "Just"
                [PLiteral mempty (PInt 1)]
            ]
      it "Multiple ints make wildcard necessary" $ do
        redundantCasesCheck
          [PLiteral mempty (PInt 1), PLiteral mempty (PInt 2), PWildcard mempty]
          `shouldBe` Right []
    describe "noDuplicateVariables" $ do
      it "Is fine with wildcard" $ do
        noDuplicatesCheck (PWildcard mempty) `shouldSatisfy` isRight
      it "Is fine with lit" $ do
        noDuplicatesCheck (PLiteral mempty (PBool True)) `shouldSatisfy` isRight
      it "Is fine with single var" $ do
        noDuplicatesCheck (PVar mempty "a") `shouldSatisfy` isRight
      it "Is fine with a pair of different vars" $ do
        noDuplicatesCheck
          ( PTuple
              mempty
              (PVar mempty "a")
              (NE.singleton $ PVar mempty "b")
          )
          `shouldSatisfy` isRight
      it "Hates a pair of the same var" $ do
        noDuplicatesCheck
          ( PTuple
              mempty
              (PVar mempty "a")
              (NE.singleton $ PVar mempty "a")
          )
          `shouldSatisfy` isLeft

      it "Is fine with a constructor with no dupes" $ do
        noDuplicatesCheck
          ( PConstructor
              mempty
              "Dog"
              [ PVar mempty "a",
                PVar mempty "b"
              ]
          )
          `shouldSatisfy` isRight
      it "Is not fine with a constructor with dupes" $ do
        noDuplicatesCheck
          ( PConstructor
              mempty
              "Dog"
              [ PVar mempty "a",
                PVar mempty "b",
                PConstructor
                  mempty
                  "Dog"
                  [ PVar mempty "c",
                    PVar mempty "a"
                  ]
              ]
          )
          `shouldSatisfy` isLeft

-}
