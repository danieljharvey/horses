{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Tests.PropertyTest
  ( createPropertyTest,
    runPropertyTest,
  )
where

import Control.Monad.Except
import Data.Bifunctor
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Interpreter
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store
import Language.Mimsa.Tests.Generate
import Language.Mimsa.Tests.Helpers
import Language.Mimsa.Tests.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

-- | given a StoreExpression, check it is the correct type
-- and if so return a PropertyTest for it
-- a property test expression must have type \a -> Boolean
-- the `a` will be generated
createPropertyTest ::
  Project Annotation ->
  StoreExpression Annotation ->
  TestName ->
  Either (Error Annotation) PropertyTest
createPropertyTest project storeExpr testName = do
  let testExpr = storeExpression storeExpr

  resolvedExpr <-
    Actions.getTypecheckedStoreExpression
      (prettyPrint testExpr)
      project
      testExpr

  first (TypeErr mempty) (isRightShape (reMonoType resolvedExpr))
  let deps =
        S.fromList $
          M.elems (getBindings $ storeBindings storeExpr)
            <> M.elems (getTypeBindings $ storeTypeBindings storeExpr)
  pure $
    PropertyTest
      { ptName = testName,
        ptExprHash = getStoreExpressionHash storeExpr,
        ptDeps = deps
      }

runPropertyTest ::
  (MonadError (Error Annotation) m, MonadIO m) =>
  Project Annotation ->
  PropertyTest ->
  m (PropertyTestResult Variable Annotation)
runPropertyTest project pt = do
  -- fetch test expression
  case lookupExprHash project (ptExprHash pt) of
    Nothing ->
      throwError
        (StoreErr $ CouldNotFindStoreExpression (ptExprHash pt))
    Just se -> do
      let testExpr = storeExpression se

      -- typecheck expression
      resolvedExpr <-
        toMonadError $
          Actions.getTypecheckedStoreExpression
            (prettyPrint testExpr)
            project
            testExpr

      -- get input part of function
      inputMt <-
        toMonadError $
          first
            (TypeErr mempty)
            (getInputType (reMonoType resolvedExpr))

      -- generate inputs
      samples <- liftIO $ generateFromMonoType inputMt

      let exprs = applyGenerated (reExpression resolvedExpr) <$> samples

      -- Apply the generated value to the test function, and return a tuple
      -- of the value and the outcome
      results <-
        toMonadError $
          traverse
            ( \(sample, rExpr) ->
                (,) sample <$> first InterpreterErr (interpret (reScope resolvedExpr) (reSwaps resolvedExpr) rExpr)
            )
            exprs

      -- work out if test passed or failed
      pure (getPropertyTestResult results)

-- | root expr -> generated -> (generated, combined)
applyGenerated ::
  (Monoid ann) =>
  Expr var ann ->
  Expr var ann ->
  (Expr var ann, Expr var ann)
applyGenerated expr val =
  (val, exprEqualsTrue (MyApp mempty expr val))

-- | if all the `snd` of the tuple == True
-- then everything succeeded, otherwise return all the failures
getPropertyTestResult :: (Ord ann) => [(Expr Variable ann, Expr Name ann)] -> PropertyTestResult Variable ann
getPropertyTestResult results =
  let failures = S.filter (\(_, result) -> not $ testIsSuccess result) (S.fromList results)
   in if S.null failures
        then PropertyTestSuccess
        else PropertyTestFailures (S.map fst failures)

getInputType :: MonoType -> Either TypeError MonoType
getInputType input =
  case input of
    (MTFunction _ fn _) -> Right fn
    other ->
      Left
        ( UnificationError
            other
            ( MTFunction
                mempty
                (MTVar mempty (TVName Nothing "fn"))
                (MTPrim mempty MTBool)
            )
        )

isRightShape :: MonoType -> Either TypeError ()
isRightShape mt = do
  unifies
    mt
    ( MTFunction
        mempty
        (MTVar mempty (TVUnificationVar (-1)))
        (MTPrim mempty MTBool)
    )
