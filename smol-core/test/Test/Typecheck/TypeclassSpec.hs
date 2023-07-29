{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Typecheck.TypeclassSpec (spec) where

import Control.Monad.Identity
import Data.Bifunctor (bimap)
import Data.Either
import Data.Foldable (traverse_)
import Data.Functor
import qualified Data.Map.Strict as M
import Data.String (fromString)
import Data.Text (Text)
import Smol.Core
import Smol.Core.Helpers
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Typecheck.FromParsedExpr (fromParsedExpr)
import Smol.Core.Typecheck.Typeclass
import Test.Helpers
import Test.Hspec

evalExpr ::
  [Constraint Annotation] ->
  Text ->
  Either (TCError Annotation) (ResolvedExpr (Type ResolvedDep Annotation))
evalExpr constraints input =
  case parseExprAndFormatError input of
    Left e -> error (show e)
    Right expr ->
      case resolveExprDeps expr (getTypeclassMethodNames @() typecheckEnv) of
        Left e -> error $ "error getting method names :" <> show e
        Right resolvedExpr -> case elaborate (typecheckEnv {tceConstraints = constraints}) resolvedExpr of
          Right (typedExpr, _typeclassUses) -> pure typedExpr
          Left e -> Left e

-- | elaborate but don't do clever resolving so we can construct the
-- expectations we want
evalExprUnsafe ::
  Text ->
  Either (TCError Annotation) (ResolvedExpr (Type ResolvedDep Annotation))
evalExprUnsafe input = case parseExprAndFormatError input of
  Left e -> error (show e)
  Right expr ->
    case elaborate typecheckEnv (fromParsedExpr expr) of
      Right (typedExpr, _typeclassUses) -> pure typedExpr
      Left e -> Left e

getRight :: (Show e) => Either e a -> a
getRight (Right a) = a
getRight (Left e) = error (show e)

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
      instanceMatchesType @() [tyTuple tyInt [tyInt]] [tyTuple (tcVar "a") [tyBool]]
        `shouldBe` Left (tyInt, tyBool)

    it "Eq (a,b) matches Eq (Int, Int)" $ do
      instanceMatchesType @() [tyTuple tyInt [tyInt]] [tyTuple (tcVar "a") [tcVar "b"]]
        `shouldBe` Right
          [ Substitution (SubId (Identity "a")) tyInt,
            Substitution (SubId (Identity "b")) tyInt
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

  describe "lookupTypeclassConstraint" $ do
    it "Is not there" $ do
      lookupTypeclassConstraint @() typecheckEnv (Constraint "Eq" [tcVar "a"])
        `shouldSatisfy` isLeft

    it "Is there" $ do
      let tcEnvWithConstraint = typecheckEnv {tceConstraints = [Constraint "Eq" [tcVar "a"]]}
      lookupTypeclassConstraint @() tcEnvWithConstraint (Constraint "Eq" [tcVar "a"])
        `shouldSatisfy` isRight

  describe "Check instances" $ do
    it "Good Show instance" $ do
      checkInstance @()
        typecheckEnv
        showTypeclass
        (Constraint "Show" [tyUnit])
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
        (Constraint "Show" [tyUnit])
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
        (Constraint "Eq" [tyInt])
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
        (Constraint "Show" [tyUnit])
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
        (Constraint "Eq" [tyTuple (tcVar "a") [tcVar "b"]])
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

    it "Tuple Eq instance missing a constraint" $ do
      checkInstance @()
        typecheckEnv
        eqTypeclass
        (Constraint "Eq" [tyTuple (tcVar "a") [tcVar "b"]])
        ( Instance
            { inExpr =
                unsafeParseInstanceExpr "\\a -> \\b -> case (a,b) of ((a1, a2), (b1, b2)) -> if equals a1 b1 then equals a2 b2 else False",
              inConstraints =
                [ Constraint "Eq" [tcVar "a"]
                ]
            }
        )
        `shouldSatisfy` isLeft

  fdescribe "dedupeConstraints" $ do
    it "Empty is empty" $ do
      dedupeConstraints @() mempty `shouldBe` (mempty, mempty)

    it "One is one and gets a new name" $ do
      dedupeConstraints @() (M.singleton "oldname" (Constraint "Eq" [tyInt]))
        `shouldBe` ( [ ( TypeclassCall "newname" 1,
                         Constraint "Eq" [tyInt]
                       )
                     ],
                     M.singleton "oldname" (TypeclassCall "newname" 1)
                   )

    it "Two functions, each used twice become one of each" $ do
      dedupeConstraints @()
        ( M.fromList
            [ ("eqInt1", Constraint "Eq" [tyInt]),
              ("eqInt2", Constraint "Eq" [tyInt]),
              ("eqBool1", Constraint "Eq" [tyBool]),
              ("eqBool2", Constraint "Eq" [tyBool])
            ]
        )
        `shouldBe` ( [ ( TypeclassCall "newname" 1,
                         Constraint "Eq" [tyInt]
                       ),
                       ( TypeclassCall "newname" 2,
                         Constraint "Eq" [tyBool]
                       )
                     ],
                     M.fromList
                       [ ("eqInt1", TypeclassCall "newname" 1),
                         ("eqInt2", TypeclassCall "newname" 1),
                         ("eqBool1", TypeclassCall "newname" 2),
                         ("eqBool2", TypeclassCall "newname" 2)
                       ]
                   )

  fdescribe "Inline typeclass functions" $ do
    let simplify :: Expr ResolvedDep ann -> Expr ResolvedDep ()
        simplify = void . goExpr
          where
            changeIdent (TypeclassCall ident i) =
              LocalDefinition $ "tc" <> ident <> fromString (show i)
            changeIdent (UniqueDefinition ident i) =
              LocalDefinition $ ident <> fromString (show i)
            changeIdent ident = ident

            goExpr (EVar ann ident) =
              EVar ann (changeIdent ident)
            goExpr (EAnn ann ty rest) =
              EAnn ann (typeForComparison ty) (goExpr rest)
            goExpr (EPatternMatch ann matchExpr pats) =
              EPatternMatch ann (goExpr matchExpr) (fmap (bimap goPattern goExpr) pats)
            goExpr other = mapExpr goExpr other

            goPattern (PVar ann ident) = PVar ann (changeIdent ident)
            goPattern other = mapPattern goPattern other

    traverse_
      ( \(typeclasses, parts, expectedParts) ->
          let input = joinText parts
              expected = joinText expectedParts
           in it ("Successfully inlined " <> show input) $ do
                let expr = getRight $ evalExpr (M.elems typeclasses) input
                tracePrettyM "expr" expr
                
                let env = typecheckEnv { tceConstraints = M.elems typeclasses }

                let expectedExpr = getRight $ evalExprUnsafe expected
                    result = inlineTypeclassFunctions env typeclasses expr

                tracePrettyM "expected" expectedExpr

                simplify <$> result `shouldBe` Right (simplify expectedExpr)
      )
      [ (mempty, ["1 + 2"], ["1 + 2"]),
        ( M.singleton (TypeclassCall "equals" 1) (Constraint "Eq" [tyInt]),
          ["equals (1: Int) (2: Int)"],
          [ "\\instances -> case (instances : Int -> Int -> Bool) of tcnewname1 ->",
            "tcnewname1 (1 : Int) (2 : Int)"
          ]
        ),
        ( M.fromList
            [ (TypeclassCall "equals" 1, Constraint "Eq" [tyInt]),
              (TypeclassCall "equals" 2, Constraint "Eq" [tyInt])
            ],
          ["if equals (1: Int) (2: Int) then equals (2: Int) (3: Int) else False"],
          [ "\\instances -> case (instances : Int -> Int -> Bool) of tcnewname1 ->",
            "if tcnewname1 (1 : Int) (2 : Int) then tcnewname1 (2: Int) (3: Int) else False"
          ]
        ),
        ( M.fromList [("eqA", Constraint "Eq" [tcVar "a"]), ("eqB", Constraint "Eq" [tcVar "b"])],
          [ "(\\a -> \\b -> case (a,b) of ((a1, b1), (a2, b2)) -> ",
            "if equals a1 a2 then equals b1 b2 else False : (a,b) -> (a,b) -> Bool)"
          ],
          [ "\\instances -> case (instances : (a -> a -> Bool, b -> b -> Bool)) of (tcnewname1, tcnewname2) -> ",
            "(\\a -> \\b -> case (a,b) of ((a1, a2), (b1, b2)) ->",
            "if tcnewname1 a1 b1 then tcnewname2 b1 b2 else False : (a,b) -> (a,b) -> Bool)"
          ]
        )
      ]
