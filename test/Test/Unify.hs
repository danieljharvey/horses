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
spec =
  describe "Unify" $ do
    it "Two same things teach us nothing" $
      runUnifier (MTPrim MTUnit, MTPrim MTUnit) `shouldBe` Right mempty
    it "Combines a known with an unknown" $
      runUnifier (MTVar (tvFree 1), MTPrim MTInt)
        `shouldBe` Right (Substitutions $ M.singleton (NumberedVar 1) (MTPrim MTInt))
    it "Combines two half pairs" $
      runUnifier
        ( MTPair (MTVar (tvFree 1)) (MTPrim MTInt),
          MTPair (MTPrim MTBool) (MTVar (tvFree 2))
        )
        `shouldBe` Right
          ( Substitutions $
              M.fromList
                [ (NumberedVar 1, MTPrim MTBool),
                  (NumberedVar 2, MTPrim MTInt)
                ]
          )
    it "Combines two half records" $
      runUnifier
        ( MTRecord $
            M.fromList
              [ (mkName "one", MTPrim MTInt),
                (mkName "two", MTVar (tvFree 1))
              ],
          MTRecord $
            M.fromList
              [ (mkName "one", MTVar (tvFree 2)),
                (mkName "two", MTPrim MTBool)
              ]
        )
        `shouldBe` Right
          ( Substitutions $
              M.fromList
                [ (NumberedVar 1, MTPrim MTBool),
                  (NumberedVar 2, MTPrim MTInt)
                ]
          )
    it "Combines two records" $
      runUnifier
        ( MTRecord $
            M.fromList
              [ (mkName "one", MTPrim MTInt)
              ],
          MTRecord $
            M.fromList
              [ (mkName "two", MTPrim MTBool)
              ]
        )
        `shouldBe` Right
          ( Substitutions $
              M.fromList
                [ (NumberedVar 1, MTPrim MTInt),
                  (NumberedVar 2, MTPrim MTBool)
                ]
          )
