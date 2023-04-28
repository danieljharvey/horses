{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Modules.ModulesSpec (spec) where

import Data.Bifunctor (second)
import Data.Either (isRight)
import Data.FileEmbed
import Data.Foldable (find)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Smol.Core
import Smol.Core.Modules.FromParts
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Modules.Typecheck
import Smol.Core.Types.Module.DefIdentifier
import Smol.Core.Types.Module.Module
import Smol.Core.Types.Module.ModuleHash
import Test.Helpers
import Test.Hspec

-- these are saved in a file that is included in compilation
testInputs :: [(FilePath, Text)]
testInputs =
  fmap (second T.decodeUtf8) $(makeRelativeToProject "test/static/" >>= embedDir)

preludeInput :: Text
preludeInput = case find (\(filename, _) -> "Prelude" `isInfixOf` filename) testInputs of
  Just (_, code) -> code
  Nothing -> error "could not find prelude code"

spec :: Spec
spec = do
  fdescribe "Modules" $ do
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

        resolveModuleDeps mod' `shouldBe` expected

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

        resolveModuleDeps mod' `shouldBe` expected

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

        resolveModuleDeps mod' `shouldBe` expected

      it "'main' uses a type dep from 'Maybe'" $ do
        let mod' = unsafeParseModule "type Maybe a = Just a\ndef main = let a = 456 in Just a"
            mainExpr =
              ELet
                ()
                (UniqueDefinition "a" 1)
                (nat 456)
                ( EApp
                    ()
                    (EConstructor () (LocalDefinition "Just"))
                    (EVar () (UniqueDefinition "a" 1))
                )

            expected =
              mempty
                { moExpressions =
                    M.singleton (DIName "main") mainExpr,
                  moDataTypes =
                    M.singleton
                      "Maybe"
                      ( DataType
                          { dtName = "Maybe",
                            dtVars = ["a"],
                            dtConstructors = M.fromList [("Just", [TVar () (LocalDefinition "a")])]
                          }
                      )
                }

        resolveModuleDeps mod' `shouldBe` expected

    describe "Typecheck" $ do
      it "Typechecks Prelude successfully" $ do
        case parseModuleAndFormatError preludeInput of
          Right moduleParts -> do
            case resolveModuleDeps <$> moduleFromModuleParts mempty moduleParts of
              Left e -> error (show e)
              Right myModule -> do
                let modules = M.singleton (ModuleHash "123") myModule
                typecheckAllModules modules "" myModule `shouldSatisfy` isRight
          Left e -> error (show e)
