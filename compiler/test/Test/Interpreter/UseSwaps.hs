{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Interpreter.UseSwaps
  ( spec,
  )
where

import qualified Data.Map as M
import Language.Mimsa.Interpreter.UseSwaps
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec =
  describe "Use swaps" $ do
    it "Replaces basic" $ do
      let expr' = MyLambda mempty (Identifier mempty $ NumberedVar 1) (int 1)
          swaps = M.singleton (NumberedVar 1) "a"
      useSwaps swaps expr' `shouldBe` Right (MyLambda () (Identifier mempty "a") (int 1))
