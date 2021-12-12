{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Tests.PropertyTest
  ( createPropertyTest,
  )
where

import Data.Bifunctor
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Tests.Helpers
import Language.Mimsa.Tests.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

-- | a property test must have type \a -> Boolean
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

isRightShape :: MonoType -> Either TypeError ()
isRightShape mt = do
  unifies
    mt
    ( MTFunction
        mempty
        (MTVar mempty (TVName Nothing "a"))
        (MTPrim mempty MTBool)
    )
