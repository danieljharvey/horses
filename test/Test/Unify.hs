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
      runUnifier (MTVar (tvBound 1), MTInt)
        `shouldBe` Right (Substitutions $ M.singleton (UniVar 1) MTInt)
    it "Combines two half pairs" $ do
      runUnifier
        ( MTPair (MTVar (tvBound 1)) MTInt,
          MTPair MTBool (MTVar (tvBound 2))
        )
        `shouldBe` Right
          ( Substitutions $
              M.fromList
                [ (UniVar 1, MTBool),
                  (UniVar 2, MTInt)
                ]
          )
    it "Combines two half records" $ do
      runUnifier
        ( MTRecord $
            M.fromList
              [ (mkName "one", MTInt),
                (mkName "two", MTVar (tvBound 1))
              ],
          MTRecord $
            M.fromList
              [ (mkName "one", MTVar (tvBound 2)),
                (mkName "two", MTBool)
              ]
        )
        `shouldBe` Right
          ( Substitutions $
              M.fromList
                [ (UniVar 2, MTBool),
                  (UniVar 1, MTInt)
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
                [ (UniVar 1, MTInt),
                  (UniVar 2, MTBool)
                ]
          )
    it "Instantiates a free var from a bound one" $ do
      runUnifier (MTInt, MTVar (TVBound 1))
        `shouldBe` Right (Substitutions $ M.singleton (UniVar 1) MTInt)
