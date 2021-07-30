{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Server.ExpressionData
  ( ExpressionData (..),
    UnitTestData (..),
    expressionDataHandler,
    mkUnitTestData,
  )
where

import qualified Data.Aeson as JSON
import Data.Coerce
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Swagger
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Project.UnitTest
import Language.Mimsa.Store
import Language.Mimsa.Typechecker.AnnotateExpression
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker
import Servant

data UnitTestData = UnitTestData
  { utdTestName :: Text,
    utdTestSuccess :: Bool,
    utdBindings :: Map Name Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

mkUnitTestData :: Project ann -> UnitTest -> UnitTestData
mkUnitTestData project unitTest = do
  let getDep = (`findBindingNameForExprHash` project)
  let depMap = mconcat (getDep <$> S.toList (utDeps unitTest))
  UnitTestData
    (coerce $ utName unitTest)
    (coerce $ utSuccess unitTest)
    (coerce <$> depMap)

data RuntimeData = RuntimeData
  { rtdName :: Text,
    rtdDescription :: Text,
    rtdMonoType :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

toRuntimeData :: Runtime code -> RuntimeData
toRuntimeData rt =
  RuntimeData
    { rtdName = coerce (rtName rt),
      rtdDescription = rtDescription rt,
      rtdMonoType = prettyPrint (rtMonoType rt)
    }

data ExpressionType = ExpressionType
  { etName :: Text,
    etType :: Text,
    etAnnotation :: Annotation
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

expressionTypeFromTypedVariable :: TypedVariable -> ExpressionType
expressionTypeFromTypedVariable (bindingName, mt, ann) =
  ExpressionType (coerce bindingName) (prettyPrint mt) ann

data ExpressionData = ExpressionData
  { edHash :: Text,
    edPretty :: Text,
    edType :: Text,
    edBindings :: Map Name Text,
    edTypeBindings :: Map TyCon Text,
    edUnitTests :: [UnitTestData],
    edRuntimes :: Map RuntimeName RuntimeData,
    edGraphviz :: Text,
    edExpressionTypes :: [ExpressionType]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

expressionDataHandler ::
  Project Annotation ->
  StoreExpression Annotation ->
  MonoType ->
  [Graphviz] ->
  [TypedVariable] ->
  Handler ExpressionData
expressionDataHandler project se mt gv ets = do
  let exprHash = getStoreExpressionHash se
      tests =
        mkUnitTestData project
          <$> M.elems
            (getTestsForExprHash project exprHash)
      matchingRuntimes =
        toRuntimeData
          <$> getValidRuntimes mt
  pure $
    ExpressionData
      (prettyPrint exprHash)
      (prettyPrint (storeExpression se))
      (prettyPrint mt)
      (prettyPrint <$> getBindings (storeBindings se))
      (prettyPrint <$> getTypeBindings (storeTypeBindings se))
      tests
      matchingRuntimes
      (prettyGraphviz gv)
      (expressionTypeFromTypedVariable <$> ets)
