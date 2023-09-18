{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Typecheck.TypeclassSpec (spec) where

import Data.Either
import qualified Data.Map.Strict as M
import Smol.Core
import Smol.Core.Typecheck.Typeclass
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = do
  describe "recoverTypeclassUses" $ do
    it "No classes, nothing to find" $ do
      recoverTypeclassUses @() [] `shouldBe` mempty
    it "Uses Eq Int" $ do
      recoverTypeclassUses @()
        [ TCWTypeclassUse (UniqueDefinition "a" 123) "Eq" [("a", 10)],
          TCWSubstitution (Substitution (SubUnknown 10) tyInt)
        ]
        `shouldBe` M.singleton (UniqueDefinition "a" 123) (Constraint "Eq" [tyInt])

  describe "instanceMatchesType" $ do
    it "Eq (a,Bool) does not match Eq (Int, Int)" $ do
      instanceMatchesType @_ @() [tyTuple tyInt [tyInt]] [tyTuple (tcVar "a") [tyBool]]
        `shouldBe` Left (tyInt, tyBool)

    it "Eq (a,b) matches Eq (Int, Int)" $ do
      instanceMatchesType @_ @() [tyTuple tyInt [tyInt]] [tyTuple (tcVar "a") [tcVar "b"]]
        `shouldBe` Right
          [ Substitution (SubId (LocalDefinition "a")) tyInt,
            Substitution (SubId (LocalDefinition "b")) tyInt
          ]

  describe "lookupTypeclassInstance" $ do
    it "Is not there" $ do
      lookupTypeclassInstance @() typecheckEnv (Constraint "Eq" [tyBool])
        `shouldSatisfy` isLeft

    it "Is there" $ do
      let result = lookupTypeclassInstance @() typecheckEnv (Constraint "Eq" [tyInt])
      inConstraints <$> result `shouldBe` Right []

    it "Nested item is there" $ do
      let result = lookupTypeclassInstance @() typecheckEnv (Constraint "Eq" [tyTuple tyInt [tyInt]])
      inConstraints <$> result `shouldBe` Right [Constraint "Eq" [tyInt], Constraint "Eq" [tyInt]]

    it "Doubly nested item is there" $ do
      let result = lookupTypeclassInstance @() typecheckEnv (Constraint "Eq" [tyTuple tyInt [tyTuple tyInt [tyInt]]])
      result `shouldSatisfy` isRight

    it "Other nested item is there" $ do
      lookupTypeclassInstance @() typecheckEnv (Constraint "Eq" [tyTuple tyBool [tyInt]])
        `shouldSatisfy` isLeft

  describe "Check instances" $ do
    it "Good Show instance" $ do
      checkInstance @()
        typecheckEnv
        showTypeclass
        (addTypesToConstraint (Constraint "Show" [tyUnit]))
        ( Instance
            { inExpr = unsafeParseInstanceExpr "\\a -> \"Unit\"",
              inConstraints = []
            }
        )
        `shouldSatisfy` isRight

    it "Bad Show instance" $ do
      checkInstance @()
        typecheckEnv
        showTypeclass
        (addTypesToConstraint (Constraint "Show" [tyUnit]))
        ( Instance
            { inExpr = unsafeParseInstanceExpr "\\a -> 123",
              inConstraints = []
            }
        )
        `shouldSatisfy` isLeft

    it "Good Eq instance" $ do
      checkInstance @()
        typecheckEnv
        eqTypeclass
        (addTypesToConstraint (Constraint "Eq" [tyInt]))
        ( Instance
            { inExpr = unsafeParseInstanceExpr "\\a -> \\b -> a == b",
              inConstraints = []
            }
        )
        `shouldSatisfy` isRight

    it "Bad Eq instance" $ do
      checkInstance @()
        typecheckEnv
        eqTypeclass
        (addTypesToConstraint (Constraint "Show" [tyUnit]))
        ( Instance
            { inExpr = unsafeParseInstanceExpr "\\a -> \\b -> 123",
              inConstraints = []
            }
        )
        `shouldSatisfy` isLeft

    it "Tuple Eq instance" $ do
      checkInstance @()
        typecheckEnv
        eqTypeclass
        (addTypesToConstraint (Constraint "Eq" [tyTuple (tcVar "a") [tcVar "b"]]))
        ( Instance
            { inExpr =
                unsafeParseInstanceExpr "\\a -> \\b -> case (a,b) of ((a1, a2), (b1, b2)) -> if equals a1 b1 then equals a2 b2 else False",
              inConstraints =
                [ Constraint "Eq" [tcVar "a"],
                  Constraint "Eq" [tcVar "b"]
                ]
            }
        )
        `shouldSatisfy` isRight

    it "Natural Show instance" $ do
      checkInstance @()
        typecheckEnv
        showTypeclass
        (addTypesToConstraint (Constraint "Show" [tyCons "Natural" []]))
        ( Instance
            { inExpr =
                unsafeParseInstanceExpr "\\nat -> case nat of Suc n -> \"S \" + show n | _ -> \"\"",
              inConstraints =
                []
            }
        )
        `shouldSatisfy` isRight

    fit "Functor Maybe instance" $ do
      checkInstance @()
        typecheckEnv
        functorTypeclass
        (addTypesToConstraint (Constraint "Functor" [tyCons "Maybe" []]))
        ( Instance
            { inExpr =
                unsafeParseInstanceExpr "\\f -> \\maybe -> case maybe of Just a -> Just (f a) | Nothing -> Nothing",
              inConstraints = mempty
            }
        )
        `shouldSatisfy` isRight

  -- don't do anything with concrete ones pls
  -- then we can look those up again later
  describe "findDedupedConstraints" $ do
    it "Empty is empty" $ do
      findDedupedConstraints @() mempty `shouldBe` (mempty, mempty)

    it "One is one and gets a new name" $ do
      findDedupedConstraints @() (M.singleton "oldname" (Constraint "Eq" [tcVar "a"]))
        `shouldBe` ( [ Constraint "Eq" [tcVar "a"]
                     ],
                     M.singleton "oldname" (TypeclassCall "valuefromdictionary" 0)
                   )

    it "We don't rename concrete instances" $ do
      findDedupedConstraints @() (M.singleton "oldname" (Constraint "Eq" [tyInt]))
        `shouldBe` ( mempty,
                     mempty
                   )

    it "Two functions, each used twice become one of each" $ do
      findDedupedConstraints @()
        ( M.fromList
            [ ("eqInt1", Constraint "Eq" [tcVar "a"]),
              ("eqInt2", Constraint "Eq" [tcVar "a"]),
              ("eqBool1", Constraint "Eq" [tcVar "b"]),
              ("eqBool2", Constraint "Eq" [tcVar "b"])
            ]
        )
        `shouldBe` ( [ Constraint "Eq" [tcVar "a"],
                       Constraint "Eq" [tcVar "b"]
                     ],
                     M.fromList
                       [ ("eqBool1", TypeclassCall "valuefromdictionary" 0),
                         ("eqBool2", TypeclassCall "valuefromdictionary" 0),
                         ("eqInt1", TypeclassCall "valuefromdictionary" 1),
                         ("eqInt2", TypeclassCall "valuefromdictionary" 1)
                       ]
                   )

  describe "matchType" $ do
    it "(Int, Bool) matches (a,b)" $ do
      let tyMatch = unsafeParseType "(Int, Bool)"
          tyTypeclass = unsafeParseType "(a,b)"
      matchType tyMatch tyTypeclass `shouldSatisfy` isRight

    it "[Int] matches [a]" $ do
      let tyMatch = unsafeParseType "[Int]"
          tyTypeclass = unsafeParseType "[a]"
      matchType tyMatch tyTypeclass `shouldSatisfy` isRight

    it "Horse matches Horse" $ do
      let tyMatch = unsafeParseType "Horse"
          tyTypeclass = unsafeParseType "Horse"
      matchType tyMatch tyTypeclass `shouldSatisfy` isRight

    it "Maybe Int matches Maybe a" $ do
      let tyMatch = unsafeParseType "Maybe Int"
          tyTypeclass = unsafeParseType "Maybe a"
      matchType tyMatch tyTypeclass `shouldSatisfy` isRight

  describe "isConcrete" $ do
    it "yes, because it has no vars" $ do
      isConcrete @_ @() (Constraint "Eq" [tyInt]) `shouldBe` True

    it "no, because it has a var" $ do
      isConcrete @_ @() (Constraint "Eq" [tcVar "a"]) `shouldBe` False
