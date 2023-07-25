{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Typecheck.TypeclassSpec (spec) where

import Control.Monad.Identity
import Data.Either
import Data.Functor
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Smol.Core
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Typecheck.Typeclass
import Test.Helpers
import Test.Hspec

evalExpr ::
  Text ->
  Either (TCError Annotation) (ResolvedExpr (Type ResolvedDep Annotation))
evalExpr input = case parseExprAndFormatError input of
  Left e -> error (show e)
  Right expr -> testElaborate expr

getRight :: (Show e) => Either e a -> a
getRight (Right a) = a
getRight (Left e) = error (show e)

testElaborate ::
  (Ord ann, Show ann, Monoid ann) =>
  Expr ParseDep ann ->
  Either (TCError ann) (Expr ResolvedDep (Type ResolvedDep ann))
testElaborate expr =
  case resolveExprDeps expr mempty of
    Left e -> error (show e)
    Right resolvedExpr -> case elaborate typecheckEnv resolvedExpr of
      Right (typedExpr, _typeclassUses) -> pure typedExpr
      Left e -> Left e

spec :: Spec
spec = do
  describe "TypeclassSpec" $ do
    describe "recoverTypeclassUses" $ do
      it "No classes, nothing to find" $ do
        recoverTypeclassUses @() [] `shouldBe` mempty
      it "Uses Eq Int" $ do
        recoverTypeclassUses @()
          [ TCWTypeclassUse (UniqueDefinition "a" 123) "Eq" [("a", 10)],
            TCWSubstitution (Substitution (SubUnknown 10) tyInt)
          ]
          `shouldBe` M.singleton (UniqueDefinition "a" 123) (TypeclassHead "Eq" [tyInt])

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
        lookupTypeclassInstance @() typecheckEnv (TypeclassHead "Eq" [tyBool])
          `shouldSatisfy` isLeft

      it "Is there" $ do
        let result = lookupTypeclassInstance @() typecheckEnv (TypeclassHead "Eq" [tyInt])
        inConstraints <$> result `shouldBe` Right []

      it "Nested item is there" $ do
        let result = lookupTypeclassInstance @() typecheckEnv (TypeclassHead "Eq" [tyTuple tyInt [tyInt]])
        inConstraints <$> result `shouldBe` Right [TypeclassHead "Eq" [tyInt], TypeclassHead "Eq" [tyInt]]

      it "Doubly nested item is there" $ do
        let result = lookupTypeclassInstance @() typecheckEnv (TypeclassHead "Eq" [tyTuple tyInt [tyTuple tyInt [tyInt]]])
        result `shouldSatisfy` isRight

      it "Other nested item is there" $ do
        lookupTypeclassInstance @() typecheckEnv (TypeclassHead "Eq" [tyTuple tyBool [tyInt]])
          `shouldSatisfy` isLeft

    describe "lookupTypeclassConstraint" $ do
      it "Is not there" $ do
        lookupTypeclassConstraint @() typecheckEnv (TypeclassHead "Eq" [tcVar "a"])
          `shouldSatisfy` isLeft

      it "Is there" $ do
        let tcEnvWithConstraint = typecheckEnv {tceConstraints = [TypeclassHead "Eq" [tcVar "a"]]}
        lookupTypeclassConstraint @() tcEnvWithConstraint (TypeclassHead "Eq" [tcVar "a"])
          `shouldSatisfy` isRight

    describe "Check instances" $ do
      it "Good Show instance" $ do
        checkInstance @()
          typecheckEnv
          showTypeclass
          (TypeclassHead "Show" [tyUnit])
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
          (TypeclassHead "Show" [tyUnit])
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
          (TypeclassHead "Eq" [tyInt])
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
          (TypeclassHead "Show" [tyUnit])
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
          (TypeclassHead "Eq" [tyTuple (tcVar "a") [tcVar "b"]])
          ( Instance
              { inExpr =
                  unsafeParseInstanceExpr "\\a -> \\b -> case (a,b) of ((a1, a2), (b1, b2)) -> if equals a1 b1 then equals a2 b2 else False",
                inConstraints =
                  [ TypeclassHead "Eq" [tcVar "a"],
                    TypeclassHead "Eq" [tcVar "b"]
                  ]
              }
          )
          `shouldSatisfy` isRight

      it "Tuple Eq instance missing a constraint" $ do
        checkInstance @()
          typecheckEnv
          eqTypeclass
          (TypeclassHead "Eq" [tyTuple (tcVar "a") [tcVar "b"]])
          ( Instance
              { inExpr =
                  unsafeParseInstanceExpr "\\a -> \\b -> case (a,b) of ((a1, a2), (b1, b2)) -> if equals a1 b1 then equals a2 b2 else False",
                inConstraints =
                  [ TypeclassHead "Eq" [tcVar "a"]
                  ]
              }
          )
          `shouldSatisfy` isLeft

    describe "Inline typeclass functions" $ do
      it "No functions, no change" $ do
        let expr = getRight $ evalExpr "1 + 2"
            expected = getRight $ evalExpr "1 + 2"

        inlineTypeclassFunctions typecheckEnv mempty expr
          `shouldBe` Right expected

      it "Eq Int functions inlined" $ do
        let expr = getRight $ evalExpr "equals (1: Int) (2: Int)"
            expected = void $ getRight $ evalExpr "let equals = (\\a -> \\b -> a == b : Int -> Int -> Bool); equals (1 : Int) (2 : Int)"
            typeclasses = M.singleton "equals" (TypeclassHead "Eq" [tyInt])

        fmap void (inlineTypeclassFunctions typecheckEnv typeclasses expr)
          `shouldBe` Right expected
