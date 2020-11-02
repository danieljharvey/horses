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
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
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
      runUnifier (MTPrim mempty MTUnit, MTPrim mempty MTUnit) `shouldBe` Right mempty
    it "Combines a known with an unknown" $
      runUnifier (MTVar mempty (tvFree 1), MTPrim mempty MTInt)
        `shouldBe` Right (Substitutions $ M.singleton (NumberedVar 1) (MTPrim mempty MTInt))
    it "Combines two half pairs" $
      runUnifier
        ( MTPair mempty (MTVar mempty (tvFree 1)) (MTPrim mempty MTInt),
          MTPair mempty (MTPrim mempty MTBool) (MTVar mempty (tvFree 2))
        )
        `shouldBe` Right
          ( Substitutions $
              M.fromList
                [ (NumberedVar 1, MTPrim mempty MTBool),
                  (NumberedVar 2, MTPrim mempty MTInt)
                ]
          )
    it "Combines two half records" $
      runUnifier
        ( MTRecord mempty $
            M.fromList
              [ (mkName "one", MTPrim mempty MTInt),
                (mkName "two", MTVar mempty (tvFree 1))
              ],
          MTRecord mempty $
            M.fromList
              [ (mkName "one", MTVar mempty (tvFree 2)),
                (mkName "two", MTPrim mempty MTBool)
              ]
        )
        `shouldBe` Right
          ( Substitutions $
              M.fromList
                [ (NumberedVar 1, MTPrim mempty MTBool),
                  (NumberedVar 2, MTPrim mempty MTInt)
                ]
          )
    it "Combines two records" $
      runUnifier
        ( MTRecord mempty $
            M.fromList
              [ (mkName "one", MTPrim mempty MTInt)
              ],
          MTRecord mempty $
            M.fromList
              [ (mkName "two", MTPrim mempty MTBool)
              ]
        )
        `shouldBe` Right
          ( Substitutions $
              M.fromList
                [ (NumberedVar 1, MTPrim mempty MTInt),
                  (NumberedVar 2, MTPrim mempty MTBool)
                ]
          )
