{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Typecheck.ToDictionaryPassingSpec (spec) where

import Control.Monad.Reader
import Data.Bifunctor (bimap)
import Data.Foldable (traverse_)
import Data.Functor
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.String (fromString)
import qualified Data.Text as T
import Smol.Core
import Smol.Core.Typecheck.FromParsedExpr (fromParsedExpr)
import Smol.Core.Typecheck.Typeclass
import Smol.Modules.ResolveDeps
import Smol.Modules.Types.Module
import Test.Helpers
import Test.Hspec

simplify :: Expr ResolvedDep ann -> Expr ResolvedDep ()
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
    goExpr (ELambda ann ident body) =
      ELambda ann (changeIdent ident) (goExpr body)
    goExpr (EPatternMatch ann matchExpr pats) =
      EPatternMatch ann (goExpr matchExpr) (fmap (bimap goPattern goExpr) pats)
    goExpr other = mapExpr goExpr other

    goPattern (PVar ann ident) = PVar ann (changeIdent ident)
    goPattern other = mapPattern goPattern other

constructorsForTypecheckEnv :: TCEnv ann -> S.Set Constructor
constructorsForTypecheckEnv env =
  foldMap (M.keysSet . dtConstructors) (tceDataTypes env)

evalExpr ::
  [Constraint ResolvedDep Annotation] ->
  T.Text ->
  Either
    (TCError Annotation)
    ( ResolvedExpr (Type ResolvedDep Annotation),
      M.Map (ResolvedDep Identifier) (Constraint ResolvedDep Annotation)
    )
evalExpr constraints input =
  case parseExprAndFormatError input of
    Left e -> error (show e)
    Right expr ->
      case resolveExprDeps expr (getTypeclassMethodNames @Annotation typecheckEnv) mempty (constructorsForTypecheckEnv @Annotation typecheckEnv) of
        Left e -> error $ "error resolving Expr deps :" <> show e
        Right resolvedExpr ->
          let env =
                typecheckEnv
                  { tceConstraints = constraints
                  }
           in elaborate env resolvedExpr

-- | elaborate but don't do clever resolving so we can construct the
-- expectations we want
evalExprUnsafe ::
  T.Text ->
  Either (TCError Annotation) (ResolvedExpr (Type ResolvedDep Annotation))
evalExprUnsafe input = case parseExprAndFormatError input of
  Left e -> error (show e)
  Right expr ->
    case elaborate typecheckEnv (fromParsedExpr expr) of
      Right (typedExpr, _typeclassUses) -> pure typedExpr
      Left e -> Left e

runDictEnv :: ReaderT PassDictEnv m a -> m a
runDictEnv = flip runReaderT emptyPassDictEnv

spec :: Spec
spec = do
  describe "toDictionaryPassing" $ do
    describe "Get dictionaries" $ do
      it "Single item dictionary for single constraint" $ do
        let constraints = addTypesToConstraint <$> NE.fromList [Constraint "Eq" [tyInt]]
            expected = evalExprUnsafe "(\\a1 -> \\b2 -> a1 == b2 : Int -> Int -> Bool)"

            instances = moInstances testModule
            dictEnv =
              ToDictEnv
                { tdeClasses = tceClasses typecheckEnv,
                  tdeInstances = instances,
                  tdeVars = mempty
                }

        fmap simplify (runDictEnv $ createTypeclassDict dictEnv constraints)
          `shouldBe` simplify <$> expected

      it "Tuple for two constraints" $ do
        let constraints = addTypesToConstraint <$> NE.fromList [Constraint "Eq" [tyInt], Constraint "Eq" [tyInt]]
            expected = evalExprUnsafe "((\\a1 -> \\b2 -> a1 == b2 : Int -> Int -> Bool), (\\a1 -> \\b2 -> a1 == b2 : Int -> Int -> Bool))"

            instances = moInstances testModule
            dictEnv =
              ToDictEnv
                { tdeClasses = tceClasses typecheckEnv,
                  tdeInstances = instances,
                  tdeVars = mempty
                }

        fmap simplify (runDictEnv $ createTypeclassDict dictEnv constraints)
          `shouldBe` simplify <$> expected

    describe "Convert expr to use typeclass dictionaries" $ do
      traverse_
        ( \(constraints, parts, expectedConstraints, expectedParts) ->
            let input = joinText parts
                expected = joinText expectedParts
             in it ("Successfully converted " <> show input) $ do
                  let (expr, typeclassUses) = getRight $ evalExpr constraints input
                      instances = mempty

                  let expectedExpr = getRight $ evalExprUnsafe expected
                      (dedupedConstraints, tidyExpr) = deduplicateConstraints typeclassUses expr

                      dictEnv =
                        ToDictEnv
                          { tdeClasses = tceClasses typecheckEnv,
                            tdeInstances = instances,
                            tdeVars = mempty
                          }

                      result = runDictEnv $ convertExprToUseTypeclassDictionary dictEnv (addTypesToConstraint <$> dedupedConstraints) tidyExpr

                  dedupedConstraints `shouldBe` expectedConstraints
                  simplify <$> result `shouldBe` Right (simplify expectedExpr)
        )
        [ (mempty, ["1 + 2"], mempty, ["1 + 2"]),
          ( [ Constraint "Eq" [tcVar "a"],
              Constraint "Eq" [tcVar "b"]
            ],
            [ "(\\a -> \\b -> case (a,b) { ((leftA, leftB), (rightA, rightB)) -> ",
              "if equals leftA rightA then equals leftB rightB else False }: (a,b) -> (a,b) -> Bool)"
            ],
            [ Constraint "Eq" [tcVar "a"],
              Constraint "Eq" [tcVar "b"]
            ],
            [ "\\instances -> case (instances : (a -> a -> Bool, b -> b -> Bool)) { (tcvaluefromdictionary0, tcvaluefromdictionary1) -> ",
              "(\\a1 -> \\b2 -> case (a1,b2) { ((leftA3, leftB4), (rightA5, rightB6)) ->",
              "if tcvaluefromdictionary0 leftA3 rightA5 then tcvaluefromdictionary1 leftB4 rightB6 else False } : (a,b) -> (a,b) -> Bool)}"
            ]
          )
        ]

    -- the whole transformation basically
    describe "toDictionaryPassing" $ do
      traverse_
        ( \(constraints, parts, expectedParts) -> do
            let input = joinText parts
                expected = joinText expectedParts
             in it ("Successfully inlined " <> show input) $ do
                  let (expr, typeclassUses) = getRight $ evalExpr constraints input

                      instances = moInstances testModule

                  let expectedExpr = getRight $ evalExprUnsafe expected

                      (dedupedConstraints, tidyExpr) = deduplicateConstraints typeclassUses expr

                      allConstraints = nub (dedupedConstraints <> constraints) -- we lose outer constraints sometimes
                      dictEnv =
                        ToDictEnv
                          { tdeClasses = tceClasses typecheckEnv,
                            tdeInstances = instances,
                            tdeVars = mempty
                          }

                      result = toDictionaryPassing dictEnv mempty (addTypesToConstraint <$> allConstraints) tidyExpr

                  simplify <$> result `shouldBe` Right (simplify expectedExpr)
        )
        [ (mempty, ["1 + 2"], ["1 + 2"]),
          ( mempty,
            ["equals (1: Int) (2: Int)"],
            [ "let eqint = (\\a1 -> \\b2 -> a1 == b2 : Int -> Int -> Bool); eqint (1 : Int) (2: Int)"
            ]
          ),
          ( mempty,
            ["equals ((1: Int), (2: Int)) ((2: Int), (3: Int))"],
            [ "let eqintint = let eqint = (\\a1 -> \\b2 -> a1 == b2 : Int -> Int -> Bool);",
              "(\\pairA7 -> \\pairB8 -> case (pairA7, pairB8) {((a19, b110), (a211, b212)) ->",
              "if eqint a19 a211 ",
              "then eqint b110 b212",
              "else False }: (a, b) -> (a,b) -> Bool); ",
              "eqintint ((1: Int), (2: Int)) ((2: Int), (3: Int))"
            ]
          ),
          ( [Constraint "Eq" [tcVar "a"]],
            ["(\\a -> \\b -> equals a b : a -> a -> Bool)"],
            [ "\\instances -> case (instances : (a -> a -> Bool)) { tcvaluefromdictionary0 -> ",
              "(\\a1 -> \\b2 -> tcvaluefromdictionary0 a1 b2 : a -> a -> Bool) }"
            ]
          ),
          ( mempty,
            ["show Zero"],
            [ "let shownatural = (\\nat15 -> ",
              "case nat15 { Suc n16 -> \"S \" + shownatural n16 ",
              ", _ -> \"\" } : Natural -> String); ",
              "shownatural Zero"
            ]
          )
        ]
