{-# LANGUAGE OverloadedStrings #-}

module Test.RecordUsage
  ( spec,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Typechecker.RecordUsages
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers

getSubsForRec ::
  Expr Variable ann ->
  IO (Map TypeIdentifier MonoType)
getSubsForRec expr' = case runTcMonad mempty (getSubstitutionsForRecordUsages expr') of
  Right a -> pure (getSubstitutions a)
  Left _e -> error "oh no"

spec :: Spec
spec = do
  describe "RecordUsage" $ do
    it "Nothing in literal" $ do
      let expr' = MyLiteral () $ MyBool True
      getRecordUsages expr' `shouldBe` mempty
      subs <- getSubsForRec expr'
      M.size subs `shouldBe` 0
    it "Captures lambda variable" $ do
      let expr' =
            MyLambda
              ()
              (named "a")
              (MyLiteral () (MyBool True))
      getRecordUsages expr'
        `shouldBe` CombineMap
          (M.fromList [(named "a", mempty)])
      subs <- getSubsForRec expr'
      M.size subs `shouldBe` 0
    it "Captures two record usages" $ do
      let expr' =
            MyLambda
              ()
              (named "a")
              ( MyPair
                  ()
                  ( MyRecordAccess
                      ()
                      (MyVar () (named "a"))
                      (mkName "one")
                  )
                  ( MyRecordAccess
                      ()
                      (MyVar () (named "a"))
                      (mkName "two")
                  )
              )
      getRecordUsages expr'
        `shouldBe` CombineMap
          ( M.fromList
              [ ( named "a",
                  S.fromList [mkName "one", mkName "two"]
                )
              ]
          )
      subs <- getSubsForRec expr'
      M.size subs `shouldBe` 1
