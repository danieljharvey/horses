{-# LANGUAGE OverloadedStrings #-}

module Test.Typecheck.PatternSpec (spec) where

import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Smol.Core
import Test.BuiltInTypes
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = do
  fdescribe "checkPattern" $ do
    let emptyEnv = TCEnv mempty mempty (builtInTypes emptyResolvedDep)
    it "PVar" $ do
      snd <$> runReaderT (checkPattern tyInt (PVar () "a")) emptyEnv
        `shouldBe` Right
          ( M.singleton "a" tyInt
          )

    it "PVar with unique" $ do
      let mkUnique a = UniqueDefinition a 123
      snd <$> runReaderT (checkPattern tyInt (PVar () (mkUnique "a"))) emptyEnv
        `shouldBe` Right
          ( M.singleton (mkUnique "a") tyInt
          )

    it "PConstructor with unique" $ do
      let mkUnique a = UniqueDefinition a 123
      snd
        <$> runReaderT
          ( checkPattern
              ( TApp
                  ()
                  (TConstructor () (LocalDefinition "Expr"))
                  (TVar () (mkUnique "ann"))
              )
              (PConstructor () "EInt" [PWildcard (), PVar () (mkUnique "i")])
          )
          emptyEnv
        `shouldBe` Right
          ( M.fromList
              [ (mkUnique "i", tyInt)
              ]
          )

    it "PConstructor with unique from unknown type" $ do
      let mkUnique a = UniqueDefinition a 123
      snd
        <$> runReaderT
          ( checkPattern
              ( TUnknown () 123
              )
              (PConstructor () "EInt" [PWildcard (), PVar () (mkUnique "i")])
          )
          emptyEnv
        `shouldBe` Right
          ( M.fromList
              [ (mkUnique "i", tyInt)
              ]
          )
