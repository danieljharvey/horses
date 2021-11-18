{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Typechecker.Unify
  ( spec,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (runState)
import Data.Either (isLeft)
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
      TypecheckState 1 mempty mempty
    either' =
      runState
        (runReaderT (runExceptT (unify a b)) mempty)
        defaultState

spec :: Spec
spec =
  describe "Unify" $ do
    it "Two same things teach us nothing" $
      runUnifier (MTPrim mempty MTInt, MTPrim mempty MTInt) `shouldBe` Right mempty
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
    describe "Constructors" $ do
      it "Combines a Maybe" $ do
        runUnifier
          ( MTTypeApp mempty (MTConstructor mempty "Maybe") (unknown 1),
            MTTypeApp mempty (MTConstructor mempty "Maybe") (MTPrim mempty MTInt)
          )
          `shouldBe` Right
            ( Substitutions $
                M.fromList
                  [ (tvNumbered 1, MTPrim mempty MTInt)
                  ]
            )

      it "Combines an Either" $ do
        runUnifier
          ( MTTypeApp mempty (MTTypeApp mempty (MTConstructor mempty "Either") (unknown 1)) (MTPrim mempty MTBool),
            MTTypeApp mempty (MTTypeApp mempty (MTConstructor mempty "Either") (MTPrim mempty MTInt)) (unknown 2)
          )
          `shouldBe` Right
            ( Substitutions $
                M.fromList
                  [ (tvNumbered 1, MTPrim mempty MTInt),
                    (tvNumbered 2, MTPrim mempty MTBool)
                  ]
            )

    describe "Records" $ do
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

      it "Two conflicting RecordRows errors" $ do
        let leftItems = M.singleton "a" (MTPrim mempty MTInt)
            rightItems = M.singleton "a" (MTPrim mempty MTString)
        runUnifier
          ( MTRecordRow mempty leftItems (unknown 1),
            MTRecordRow mempty rightItems (unknown 2)
          )
          `shouldSatisfy` isLeft
      it "Combines Record with matching RecordRow" $ do
        let items = M.fromList [("a", MTPrim mempty MTInt), ("b", MTPrim mempty MTString)]
        runUnifier (MTRecordRow mempty items (unknown 3), MTRecord mempty items)
          `shouldBe` Right mempty
      it "Combines Record with RecordRow with less items" $ do
        let recordItems = M.fromList [("a", MTPrim mempty MTInt), ("b", MTPrim mempty MTString)]
            rowItems = M.fromList [("a", MTPrim mempty MTInt)]
        runUnifier (MTRecordRow mempty rowItems (unknown 3), MTRecord mempty recordItems)
          `shouldBe` Right
            ( Substitutions $
                M.fromList
                  [ (tvNumbered 3, MTRecordRow mempty (M.singleton "b" $ MTPrim mempty MTString) (unknown 1))
                  ]
            )
      it "Combines Record with less items with RecordRow" $ do
        let rowItems = M.fromList [("a", MTPrim mempty MTInt), ("b", MTPrim mempty MTString)]
            recordItems = M.fromList [("a", MTPrim mempty MTInt)]
        runUnifier (MTRecordRow mempty rowItems (unknown 3), MTRecord mempty recordItems)
          `shouldSatisfy` isLeft
      it "Combines Record with less items with nested RecordRow" $ do
        let rowOne = M.singleton "a" (MTPrim mempty MTInt)
            rowTwo = M.singleton "b" (MTPrim mempty MTString)
            recordItems = M.fromList [("a", MTPrim mempty MTInt)]
        runUnifier (MTRecordRow mempty rowOne (MTRecordRow mempty rowTwo (unknown 3)), MTRecord mempty recordItems)
          `shouldSatisfy` isLeft

      it "Combines two RecordRows with different items" $ do
        let leftItems = M.singleton "a" (MTPrim mempty MTInt)
            rightItems = M.singleton "b" (MTPrim mempty MTString)
        runUnifier
          ( MTRecordRow mempty leftItems (unknown 2),
            MTRecordRow mempty rightItems (unknown 3)
          )
          `shouldBe` Right
            ( Substitutions $
                M.fromList
                  [ (tvNumbered 2, MTRecordRow mempty rightItems (unknown 1)),
                    (tvNumbered 3, MTRecordRow mempty leftItems (unknown 1))
                  ]
            )
      it "Combines two RecordRows with some matching items" $ do
        let leftItems =
              M.fromList
                [ ("same", MTPrim mempty MTInt),
                  ("a", MTPrim mempty MTString)
                ]
            rightItems =
              M.fromList
                [ ("same", MTPrim mempty MTInt),
                  ("b", MTPrim mempty MTBool)
                ]
        runUnifier (MTRecordRow mempty leftItems (unknown 10), MTRecordRow mempty rightItems (unknown 11))
          `shouldBe` Right
            ( Substitutions $
                M.fromList
                  [ ( tvNumbered 10,
                      MTRecordRow
                        mempty
                        ( M.singleton
                            "b"
                            (MTPrim mempty MTBool)
                        )
                        (unknown 1)
                    ),
                    ( tvNumbered 11,
                      MTRecordRow
                        mempty
                        ( M.singleton
                            "a"
                            (MTPrim mempty MTString)
                        )
                        (unknown 1)
                    )
                  ]
            )
