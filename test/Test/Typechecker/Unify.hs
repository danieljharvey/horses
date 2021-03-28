{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Typechecker.Unify
  ( spec,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (runState)
import qualified Data.Map as M
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers ()
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers

runUnifier :: (MonoType, MonoType) -> Either TypeError Substitutions
runUnifier (a, b) =
  fst either'
  where
    defaultState =
      TypecheckState 1 mempty
    either' =
      runState
        (runReaderT (runExceptT (unify a b)) mempty)
        defaultState

spec :: Spec
spec =
  describe "Unify" $ do
    it "Two same things teach us nothing" $
      runUnifier (MTPrim mempty MTUnit, MTPrim mempty MTUnit) `shouldBe` Right mempty
    it "Combines a known with an unknown" $
      runUnifier (MTVar mempty (tvFree 1), MTPrim mempty MTInt)
        `shouldBe` Right (Substitutions $ M.singleton (tvNumbered 1) (MTPrim mempty MTInt))
    it "Combines two half pairs" $
      runUnifier
        ( MTPair mempty (MTVar mempty (tvFree 1)) (MTPrim mempty MTInt),
          MTPair mempty (MTPrim mempty MTBool) (MTVar mempty (tvFree 2))
        )
        `shouldBe` Right
          ( Substitutions $
              M.fromList
                [ (tvNumbered 1, MTPrim mempty MTBool),
                  (tvNumbered 2, MTPrim mempty MTInt)
                ]
          )
    it "Combines two half records" $
      runUnifier
        ( MTRecord mempty $
            M.fromList
              [ ("one", MTPrim mempty MTInt),
                ("two", MTVar mempty (tvFree 1))
              ],
          MTRecord mempty $
            M.fromList
              [ ("one", MTVar mempty (tvFree 2)),
                ("two", MTPrim mempty MTBool)
              ]
        )
        `shouldBe` Right
          ( Substitutions $
              M.fromList
                [ (tvNumbered 1, MTPrim mempty MTBool),
                  (tvNumbered 2, MTPrim mempty MTInt)
                ]
          )
    it "Turns Str into String" $
      runUnifier (MTData mempty "Str" mempty, MTVar mempty (tvNumbered 1))
        `shouldBe` Right (Substitutions $ M.singleton (tvNumbered 1) (MTPrim mempty MTString))
    it "Turns Arr into Array" $
      runUnifier (MTData mempty "Arr" [MTPrim mempty MTInt], MTVar mempty (tvNumbered 1))
        `shouldBe` Right (Substitutions $ M.singleton (tvNumbered 1) (MTArray mempty (MTPrim mempty MTInt)))
