{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.Typecheck
  ( spec,
  )
where

import Data.Functor
import qualified Data.Map as M
import qualified Language.Mimsa.Actions.Helpers.FindExistingBinding as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "Typecheck" $ do
    it "Resolves an item from the test stdlib" $ do
      let inputHash = getHashOfName testStdlib "liftA2State"
      -- get an arbitrary store expression from test project
      let storeExpr = getStoreExpression testStdlib inputHash
      -- resolve it
      let action = do
            Actions.typecheckStoreExpression storeExpr (prettyPrint storeExpr)
      -- run action
      let (newProject, _, _resolved) =
            fromRight (Actions.run testStdlib action)
      -- it should not have changed the project
      newProject `shouldBe` testStdlib

    -- this might require a lot more work
    xit "If it uses a type (but indirectly) include it in the deps" $ do
      -- this function explicitly uses Either, mentioning its type constructors
      let exprUsingType = "\\val -> match val with Right a -> a | _ -> False"
      -- this one does not, but still needs it as a dep
      let exprIndirectlyUsingType = "\\val -> useEither val"

      let action = do
            project <- Actions.getProject
            -- parse and typecheck expr 1
            res1 <- Actions.typecheckExpression project exprUsingType (unsafeParseExpr exprUsingType $> mempty)
            -- bind expr 1
            Actions.bindStoreExpression (reStoreExpression res1) "useEither"
            -- get fresh project
            project2 <- Actions.getProject
            -- parse and typecheck expr 2
            res2 <- Actions.typecheckExpression project2 exprIndirectlyUsingType (unsafeParseExpr exprIndirectlyUsingType $> mempty)
            -- bind expr 1
            Actions.bindStoreExpression (reStoreExpression res2) "useEitherIndirectly"
            -- get updated project
            projectNew <- Actions.getProject
            -- return it
            case Actions.findExistingBinding "useEitherIndirectly" projectNew of
              Just se -> pure se
              _ -> error "could not find the store expression we just made"
      let (_, _, found) = fromRight (Actions.run testStdlib action)

      M.size (getTypeBindings $ storeTypeBindings found) `shouldBe` 1
