{-# LANGUAGE OverloadedStrings #-}

module Test.Modules.ResolveDepsSpec (spec) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Smol.Core
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Modules.Types hiding (Entity (..))
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = do
  describe "Modules" $ do
    fdescribe "ResolvedDeps" $ do
      it "No deps, marks var as unique" $ do
        let mod' = unsafeParseModule "def main = let a = 123 in a"
            expr =
              ELet
                ()
                (UniqueDefinition "a" 1)
                (int 123)
                (EVar () (UniqueDefinition "a" 1))

            expected =
              mempty
                { moExpressions = M.singleton "main" (TopLevelExpression expr Nothing)
                }

        fst <$> resolveModuleDeps mempty mod' `shouldBe` Right expected

      it "Marks a typeclass usage as TypeclassCall with unique number" $ do
        let mod' = unsafeParseModule "def main = equals 1 2"
            expr =
              EApp
                ()
                ( EApp
                    ()
                    (EVar () (TypeclassCall "equals" 1))
                    (int 1)
                )
                (int 2)

            typeclassMethods = S.singleton "equals"
            expected =
              mempty
                { moExpressions = M.singleton "main" (TopLevelExpression expr Nothing)
                }

        fst <$> resolveModuleDeps typeclassMethods mod' `shouldBe` Right expected

      it "No deps, marks two different `a` values correctly" $ do
        let mod' = unsafeParseModule "def main = let a = 123 in let a = 456 in a"
            expr =
              ELet
                ()
                (UniqueDefinition "a" 1)
                (int 123)
                ( ELet
                    ()
                    (UniqueDefinition "a" 2)
                    (int 456)
                    (EVar () (UniqueDefinition "a" 2))
                )

            expected =
              mempty
                { moExpressions = M.singleton "main" (TopLevelExpression expr Nothing)
                }

        fst <$> resolveModuleDeps mempty mod' `shouldBe` Right expected

      it "Lambdas add new variables" $ do
        let mod' = unsafeParseModule "def main = \\a -> a"
            expr = ELambda () (UniqueDefinition "a" 1) (EVar () (UniqueDefinition "a" 1))

            expected =
              mempty
                { moExpressions = M.singleton "main" (TopLevelExpression expr Nothing)
                }

        fst <$> resolveModuleDeps mempty mod' `shouldBe` Right expected

      it "Variables added in pattern matches are unique" $ do
        let mod' = unsafeParseModule "def main pair = case pair of (a,_) -> a"
            expr =
              ELambda
                ()
                (UniqueDefinition "pair" 1)
                ( EPatternMatch
                    ()
                    (EVar () (UniqueDefinition "pair" 1))
                    ( NE.fromList [(PTuple () (PVar () (UniqueDefinition "a" 2)) (NE.singleton (PWildcard ())), EVar () (UniqueDefinition "a" 2))]
                    )
                )
            expected =
              mempty
                { moExpressions = M.singleton "main" (TopLevelExpression expr Nothing)
                }

        fst <$> resolveModuleDeps mempty mod' `shouldBe` Right expected

      it "'main' uses a dep from 'dep'" $ do
        let mod' = unsafeParseModule "def main = let a = dep in let a = 456 in a\ndef dep = 1"
            depExpr = int 1
            mainExpr =
              ELet
                ()
                (UniqueDefinition "a" 1)
                (EVar () (LocalDefinition "dep"))
                ( ELet
                    ()
                    (UniqueDefinition "a" 2)
                    (int 456)
                    (EVar () (UniqueDefinition "a" 2))
                )

            expected =
              mempty
                { moExpressions =
                    M.fromList
                      [ ("main", TopLevelExpression mainExpr Nothing),
                        ("dep", TopLevelExpression depExpr Nothing)
                      ]
                }

        fst <$> resolveModuleDeps mempty mod' `shouldBe` Right expected

      it "'main' uses a type dep from 'Moybe'" $ do
        let mod' = unsafeParseModule "type Moybe a = Jost a | Noothing\ndef main = let a = 456 in Jost a"
            mainExpr =
              ELet
                ()
                (UniqueDefinition "a" 1)
                (int 456)
                ( EApp
                    ()
                    (EConstructor () (LocalDefinition "Jost"))
                    (EVar () (UniqueDefinition "a" 1))
                )

            expected =
              mempty
                { moExpressions =
                    M.singleton "main" (TopLevelExpression mainExpr Nothing),
                  moDataTypes =
                    M.singleton
                      "Moybe"
                      ( DataType
                          { dtName = "Moybe",
                            dtVars = ["a"],
                            dtConstructors =
                              M.fromList
                                [ ("Jost", [TVar () (LocalDefinition "a")]),
                                  ("Noothing", mempty)
                                ]
                          }
                      )
                }

            depMap =
              M.fromList
                [ (DIName "main", S.fromList [DIType "Moybe"]),
                  (DIType "Moybe", mempty)
                ]

        resolveModuleDeps mempty mod' `shouldBe` Right (expected, depMap)
