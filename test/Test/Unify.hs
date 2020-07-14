{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Unify
  ( spec,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (runState)
import qualified Data.Map as M
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types
import Test.Helpers
import Test.Hspec

runUnifier :: (MonoType, MonoType) -> Either TypeError Substitutions
runUnifier (a, b) =
  fst either'
  where
    either' = runState (runReaderT (runExceptT (unify a b)) mempty) 1

spec :: Spec
spec = do
  describe "Unify" $ do
    it "Two same things teach us nothing" $ do
      runUnifier (MTUnit, MTUnit) `shouldBe` Right mempty
    it "Combines a known with an unknown" $ do
      runUnifier (MTVar (tvName "a"), MTInt)
        `shouldBe` Right (Substitutions $ M.singleton (tvName "a") MTInt)
    it "Combines two half pairs" $ do
      runUnifier
        ( MTPair (MTVar (tvName "a")) MTInt,
          MTPair MTBool (MTVar (tvName "b"))
        )
        `shouldBe` Right
          ( Substitutions $
              M.fromList
                [ (tvName "a", MTBool),
                  (tvName "b", MTInt)
                ]
          )
    it "Combines two half records" $ do
      runUnifier
        ( MTRecord $
            M.fromList
              [ (mkName "one", MTInt),
                (mkName "two", MTVar (tvName "a"))
              ],
          MTRecord $
            M.fromList
              [ (mkName "one", MTVar (tvName "b")),
                (mkName "two", MTBool)
              ]
        )
        `shouldBe` Right
          ( Substitutions $
              M.fromList
                [ (tvName "a", MTBool),
                  (tvName "b", MTInt)
                ]
          )
    it "Combines two records" $ do
      runUnifier
        ( MTRecord $
            M.fromList
              [ (mkName "one", MTInt)
              ],
          MTRecord $
            M.fromList
              [ (mkName "two", MTBool)
              ]
        )
        `shouldBe` Right
          ( Substitutions $
              M.fromList
                [ (TVNumber 1, MTInt),
                  (TVNumber 2, MTBool)
                ]
          )
