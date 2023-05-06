{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Modules.ModulesSpec (spec) where

import Data.Bifunctor (second)
import Data.Either (isRight)
import Data.FileEmbed
import Data.Foldable (find)
import Data.List (isInfixOf)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Smol.Core
import Smol.Core.Modules.FromParts
import Smol.Core.Modules.ModuleError
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Modules.Typecheck
import Smol.Core.Types.Module.DefIdentifier
import Smol.Core.Types.Module.Module
import Test.Helpers
import Test.Hspec

-- these are saved in a file that is included in compilation
testInputs :: [(FilePath, Text)]
testInputs =
  fmap (second T.decodeUtf8) $(makeRelativeToProject "test/static/" >>= embedDir)

getModuleInput :: String -> Text
getModuleInput moduleName = case find (\(filename, _) -> moduleName `isInfixOf` filename) testInputs of
  Just (_, code) -> code
  Nothing -> error $ "could not find " <> moduleName <> " code"

testModuleTypecheck :: String -> Either ModuleError (Module ResolvedDep (Type ResolvedDep Annotation))
testModuleTypecheck moduleName =
  case parseModuleAndFormatError (getModuleInput moduleName) of
    Right moduleParts -> do
      case moduleFromModuleParts mempty moduleParts >>= resolveModuleDeps of
        Left e -> error (show e)
        Right (myModule, deps) -> do
          typecheckModule mempty "" myModule deps
    Left e -> error (show e)

spec :: Spec
spec = do
  describe "Modules" $ do
    describe "ResolvedDeps" $ do
      it "No deps, marks var as unique" $ do
        let mod' = unsafeParseModule "def main = let a = 123 in a"
            expr =
              ELet
                ()
                (UniqueDefinition "a" 1)
                (nat 123)
                (EVar () (UniqueDefinition "a" 1))

            expected =
              mempty
                { moExpressions = M.singleton (DIName "main") expr
                }

        fst <$> resolveModuleDeps mod' `shouldBe` Right expected

      it "No deps, marks two different `a` values correctly" $ do
        let mod' = unsafeParseModule "def main = let a = 123 in let a = 456 in a"
            expr =
              ELet
                ()
                (UniqueDefinition "a" 1)
                (nat 123)
                ( ELet
                    ()
                    (UniqueDefinition "a" 2)
                    (nat 456)
                    (EVar () (UniqueDefinition "a" 2))
                )

            expected =
              mempty
                { moExpressions = M.singleton (DIName "main") expr
                }

        fst <$> resolveModuleDeps mod' `shouldBe` Right expected

      it "Lambdas add new variables" $ do
        let mod' = unsafeParseModule "def main = \\a -> a"
            expr = ELambda () (UniqueDefinition "a" 1) (EVar () (UniqueDefinition "a" 1))

            expected =
              mempty
                { moExpressions = M.singleton (DIName "main") expr
                }

        fst <$> resolveModuleDeps mod' `shouldBe` Right expected

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
                { moExpressions = M.singleton (DIName "main") expr
                }

        fst <$> resolveModuleDeps mod' `shouldBe` Right expected

      it "'main' uses a dep from 'dep'" $ do
        let mod' = unsafeParseModule "def main = let a = dep in let a = 456 in a\ndef dep = 1"
            depExpr = nat 1
            mainExpr =
              ELet
                ()
                (UniqueDefinition "a" 1)
                (EVar () (LocalDefinition "dep"))
                ( ELet
                    ()
                    (UniqueDefinition "a" 2)
                    (nat 456)
                    (EVar () (UniqueDefinition "a" 2))
                )

            expected =
              mempty
                { moExpressions =
                    M.fromList
                      [ (DIName "main", mainExpr),
                        (DIName "dep", depExpr)
                      ]
                }

        fst <$> resolveModuleDeps mod' `shouldBe` Right expected

      it "'main' uses a type dep from 'Moybe'" $ do
        let mod' = unsafeParseModule "type Moybe a = Jost a | Noothing\ndef main = let a = 456 in Jost a"
            mainExpr =
              ELet
                ()
                (UniqueDefinition "a" 1)
                (nat 456)
                ( EApp
                    ()
                    (EConstructor () (LocalDefinition "Jost"))
                    (EVar () (UniqueDefinition "a" 1))
                )

            expected =
              mempty
                { moExpressions =
                    M.singleton (DIName "main") mainExpr,
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

        resolveModuleDeps mod' `shouldBe` Right (expected, depMap)

    describe "Typecheck" $ do
      it "Typechecks Prelude successfully" $ do
        testModuleTypecheck "Prelude" `shouldSatisfy` isRight

      it "Typechecks Maybe successfully" $ do
        testModuleTypecheck "Maybe" `shouldSatisfy` isRight

      it "Typechecks Either successfully" $ do
        testModuleTypecheck "Either" `shouldSatisfy` isRight

      it "Typechecks State successfully" $ do
        testModuleTypecheck "State" `shouldSatisfy` isRight
