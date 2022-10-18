{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Typechecker.Unify
  ( spec,
  )
where

import Control.Monad.Except
import Control.Monad.State.Strict (runState)
import Data.Either (isLeft, isRight)
import qualified Data.Map.Strict as M
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Types.Typechecker.Substitutions

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
        (runExceptT (unify a b))
        defaultState

spec :: Spec
spec =
  describe "Unify" $ do
    it "Two same things teach us nothing" $
      runUnifier (MTPrim mempty MTInt, MTPrim mempty MTInt)
        `shouldBe` Right mempty

    it "Combines a known with an unknown" $
      runUnifier (MTVar mempty (TVUnificationVar 1), MTPrim mempty MTInt)
        `shouldBe` Right (Substitutions $ M.singleton (TVUnificationVar 1) (MTPrim mempty MTInt))

    it "Combines a named var with a matching named var" $
      runUnifier (MTVar mempty (TVName "a"), MTVar mempty (TVName "a"))
        `shouldSatisfy` isRight

    it "Does not combines a named var with a unification variable" $
      runUnifier (MTVar mempty (TVName "a"), MTVar mempty (TVUnificationVar 1))
        `shouldSatisfy` isLeft

    it "Combines a named/numbered var with a unification variable" $
      runUnifier (MTVar mempty (TVScopedVar 1 "a"), MTVar mempty (TVUnificationVar 1))
        `shouldSatisfy` isRight

    it "Does not combine a named var with a different named var" $
      runUnifier (MTVar mempty (TVName "a"), MTVar mempty (TVName "b"))
        `shouldSatisfy` isLeft

    it "Does not combine a named var with a concrete type" $
      runUnifier (MTVar mempty (TVName "a"), MTPrim mempty MTString)
        `shouldSatisfy` isLeft

    it "Combines a var with the same var" $
      runUnifier (MTVar mempty (TVScopedVar 1 "a"), MTVar mempty (TVScopedVar 1 "a"))
        `shouldSatisfy` isRight

    it "Does not combines a var with the same var" $
      runUnifier (MTVar mempty (TVScopedVar 2 "a"), MTVar mempty (TVScopedVar 1 "a"))
        `shouldSatisfy` isLeft

    it "Does not combines a var with a different var" $
      runUnifier (MTVar mempty (TVScopedVar 2 "b"), MTVar mempty (TVScopedVar 1 "a"))
        `shouldSatisfy` isLeft

    it "Does not unify a concrete type with a named var" $
      runUnifier (MTVar mempty (TVName "a"), MTPrim mempty MTInt)
        `shouldSatisfy` isLeft

    it "Does not unify a concrete type with a numbered var" $
      runUnifier (MTVar mempty (TVScopedVar 1 "a"), MTPrim mempty MTBool)
        `shouldSatisfy` isLeft

    it "Combines two half pairs" $
      runUnifier
        ( MTPair mempty (MTVar mempty (TVUnificationVar 1)) (MTPrim mempty MTInt),
          MTPair mempty (MTPrim mempty MTBool) (MTVar mempty (TVUnificationVar 2))
        )
        `shouldBe` Right
          ( Substitutions $
              M.fromList
                [ (TVUnificationVar 1, MTPrim mempty MTBool),
                  (TVUnificationVar 2, MTPrim mempty MTInt)
                ]
          )

    describe "Constructors" $ do
      it "Combines a Maybe" $ do
        runUnifier
          ( MTTypeApp mempty (MTConstructor mempty Nothing "Maybe") (MTVar mempty $ TVUnificationVar 1),
            MTTypeApp mempty (MTConstructor mempty Nothing "Maybe") (MTPrim mempty MTInt)
          )
          `shouldBe` Right
            ( Substitutions $
                M.fromList
                  [ (TVUnificationVar 1, MTPrim mempty MTInt)
                  ]
            )

      it "Combines an Either" $ do
        runUnifier
          ( MTTypeApp mempty (MTTypeApp mempty (MTConstructor mempty Nothing "Either") (MTVar mempty $ TVUnificationVar 1)) (MTPrim mempty MTBool),
            MTTypeApp mempty (MTTypeApp mempty (MTConstructor mempty Nothing "Either") (MTPrim mempty MTInt)) (MTVar mempty $ TVUnificationVar 2)
          )
          `shouldBe` Right
            ( Substitutions $
                M.fromList
                  [ (TVUnificationVar 1, MTPrim mempty MTInt),
                    (TVUnificationVar 2, MTPrim mempty MTBool)
                  ]
            )

    describe "Records" $ do
      it "Combines two half records" $
        runUnifier
          ( MTRecord mempty (
              M.fromList
                [ ("one", MTPrim mempty MTInt),
                  ("two", MTVar mempty (TVUnificationVar 1))
                ]) Nothing,
            MTRecord mempty (
              M.fromList
                [ ("one", MTVar mempty (TVUnificationVar 2)),
                  ("two", MTPrim mempty MTBool)
                ]) Nothing
          )
          `shouldBe` Right
            ( Substitutions $
                M.fromList
                  [ (TVUnificationVar 1, MTPrim mempty MTBool),
                    (TVUnificationVar 2, MTPrim mempty MTInt)
                  ]
            )

      it "Two conflicting RecordRows errors" $ do
        let leftItems = M.singleton "a" (MTPrim mempty MTInt)
            rightItems = M.singleton "a" (MTPrim mempty MTString)
        runUnifier
          ( MTRecord mempty leftItems (Just $ unknown 1),
            MTRecord mempty rightItems (Just $ unknown 2)
          )
          `shouldSatisfy` isLeft
      it "Combines Record with matching RecordRow" $ do
        let items = M.fromList [("a", MTPrim mempty MTInt), ("b", MTPrim mempty MTString)]
        runUnifier (MTRecord mempty items (Just $ unknown 3), MTRecord mempty items Nothing)
          `shouldBe` Right mempty
      it "Combines Record with RecordRow with less items" $ do
        let recordItems = M.fromList [("a", MTPrim mempty MTInt), ("b", MTPrim mempty MTString)]
            rowItems = M.fromList [("a", MTPrim mempty MTInt)]
        runUnifier (MTRecord mempty rowItems (Just $ unknown 3), MTRecord mempty recordItems Nothing)
          `shouldBe` Right
            ( Substitutions $
                M.fromList
                  [ (TVUnificationVar 3, MTRecord mempty (M.singleton "b" $ MTPrim mempty MTString) (Just $ unknown 1))
                  ]
            )
      it "Combines Record with less items with RecordRow" $ do
        let rowItems = M.fromList [("a", MTPrim mempty MTInt), ("b", MTPrim mempty MTString)]
            recordItems = M.fromList [("a", MTPrim mempty MTInt)]
        runUnifier (MTRecord mempty rowItems (Just $ unknown 3), MTRecord mempty recordItems Nothing)
          `shouldSatisfy` isLeft
      it "Combines Record with less items with nested RecordRow" $ do
        let rowOne = M.singleton "a" (MTPrim mempty MTInt)
            rowTwo = M.singleton "b" (MTPrim mempty MTString)
            recordItems = M.fromList [("a", MTPrim mempty MTInt)]
        runUnifier (MTRecord mempty rowOne (Just $ MTRecord mempty rowTwo (Just $ unknown 3)), MTRecord mempty recordItems Nothing)
          `shouldSatisfy` isLeft

      it "Combines two RecordRows with different items" $ do
        let leftItems = M.singleton "a" (MTPrim mempty MTInt)
            rightItems = M.singleton "b" (MTPrim mempty MTString)
        runUnifier
          ( MTRecord mempty leftItems (Just $ unknown 2),
            MTRecord mempty rightItems (Just $ unknown 3)
          )
          `shouldBe` Right
            ( Substitutions $
                M.fromList
                  [ (TVUnificationVar 2, MTRecord mempty rightItems (Just $ unknown 1)),
                    (TVUnificationVar 3, MTRecord mempty leftItems (Just $ unknown 1))
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
        runUnifier (MTRecord mempty leftItems (Just $ unknown 10), MTRecord mempty rightItems (Just $ unknown 11))
          `shouldBe` Right
            ( Substitutions $
                M.fromList
                  [ ( TVUnificationVar 10,
                      MTRecord
                        mempty
                        ( M.singleton
                            "b"
                            (MTPrim mempty MTBool)
                        )
                        (Just $ unknown 1)
                    ),
                    ( TVUnificationVar 11,
                      MTRecord
                        mempty
                        ( M.singleton
                            "a"
                            (MTPrim mempty MTString)
                        )
                        (Just $ unknown 1)
                    )
                  ]
            )
