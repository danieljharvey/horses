{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Tests.PropertyTest
  ( createPropertyTest,
    runPropertyTest,
  )
where

import Control.Monad.Except
import Data.Bifunctor
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Interpret as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store
import Language.Mimsa.Store.ResolveDataTypes
import Language.Mimsa.Tests.Generate
import Language.Mimsa.Tests.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Tests
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

  (_, _, resolvedExpr) <-
    Actions.run
      project
      ( Actions.typecheckExpression
          project
          (prettyPrint testExpr)
          testExpr
      )

  first (TypeErr mempty) (isRightShape (reMonoType resolvedExpr))
  pure $
    PropertyTest
      { ptName = testName,
        ptExprHash = getStoreExpressionHash storeExpr
      }

runPropertyTest ::
  (MonadError (Error Annotation) m, MonadIO m) =>
  Project Annotation ->
  PropertyTest ->
  m (PropertyTestResult Annotation)
runPropertyTest project pt = do
  -- fetch test expression
  case lookupExprHash project (ptExprHash pt) of
    Nothing ->
      throwError
        (StoreErr $ CouldNotFindStoreExpression (ptExprHash pt))
    Just se -> do
      let testExpr = storeExpression se

      -- typecheck expression
      (_, _, resolvedExpr) <-
        toMonadError $
          Actions.run project $
            Actions.typecheckExpression
              project
              (prettyPrint testExpr)
              testExpr

      -- get input part of function
      inputMt <-
        toMonadError $
          first
            (TypeErr mempty)
            (getInputType (reMonoType resolvedExpr))

      -- need to recursively fetch all deps of se
      seDeps <- toMonadError $ first StoreErr (recursiveResolve (prjStore project) se)

      -- generate inputs
      samples <- liftIO $ generateFromMonoType (createTypeMap seDeps) inputMt

      let exprs = applyGenerated (storeExpression . reStoreExpression $ resolvedExpr) <$> samples

      -- Apply the generated value to the test function, and return a tuple
      -- of the value and the outcome
      results <-
        toMonadError $
          traverse
            ( \(sample, rExpr) -> do
                let sampleStoreExpr = se {storeExpression = rExpr}
                (_, _, result) <-
                  toMonadError
                    ( Actions.run
                        project
                        (Actions.interpreter sampleStoreExpr)
                    )

                pure (sample, result)
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
getPropertyTestResult ::
  (Ord ann) =>
  [(Expr Name ann, Expr Name ann)] ->
  PropertyTestResult ann
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
                (MTVar mempty (TVName "fn"))
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
