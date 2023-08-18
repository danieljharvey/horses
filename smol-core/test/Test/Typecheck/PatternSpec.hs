{-# LANGUAGE OverloadedStrings #-}

module Test.Typecheck.PatternSpec (spec) where

import qualified Data.Map.Strict as M
import Smol.Core
import Test.BuiltInTypes
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = do
  describe "checkPattern" $ do
    let emptyEnv = TCEnv mempty (builtInTypes emptyResolvedDep) mempty mempty mempty
    it "PVar" $ do
      snd <$> runTypecheckM emptyEnv (checkPattern tyInt (PVar () "a"))
        `shouldBe` Right
          ( M.singleton "a" tyInt
          )

    it "PVar with unique" $ do
      let mkUnique a = UniqueDefinition a 123
      snd <$> runTypecheckM emptyEnv (checkPattern tyInt (PVar () (mkUnique "a")))
        `shouldBe` Right
          ( M.singleton (mkUnique "a") tyInt
          )

    it "PConstructor with unique" $ do
      let mkUnique a = UniqueDefinition a 123
      snd
        <$> runTypecheckM
          emptyEnv
          ( checkPattern
              ( TApp
                  ()
                  (TConstructor () (LocalDefinition "Expr"))
                  (TVar () (mkUnique "ann"))
              )
              (PConstructor () "EInt" [PWildcard (), PVar () (mkUnique "i")])
          )
        `shouldBe` Right
          ( M.fromList
              [ (mkUnique "i", tyInt)
              ]
          )

    it "PConstructor with unique from unknown type" $ do
      let mkUnique a = UniqueDefinition a 123
      snd
        <$> runTypecheckM
          emptyEnv
          ( checkPattern
              ( TUnknown () 123
              )
              (PConstructor () "EInt" [PWildcard (), PVar () (mkUnique "i")])
          )
        `shouldBe` Right
          ( M.fromList
              [ (mkUnique "i", tyInt)
              ]
          )
