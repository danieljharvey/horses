{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Server.Helpers.ExpressionData
  ( ExpressionData (..),
    UnitTestData (..),
    expressionData,
    makeExpressionData,
    mkUnitTestData,
  )
where

import Control.Monad.State
import qualified Data.Aeson as JSON
import Data.Coerce
import Data.Map (Map)
import qualified Data.Map as M
import Data.OpenApi hiding (get)
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Actions.Types
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Store
import Language.Mimsa.Tests.Test
import Language.Mimsa.Tests.Types
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Typechecker.OutputTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Project.SourceItem
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

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

data PropertyTestData = PropertyTestData
  { ptdTestName :: Text,
    ptdTestFailures :: [Text],
    ptdBindings :: Map Name Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

mkPropertyTestData ::
  Project ann ->
  PropertyTest ->
  PropertyTestResult Variable ann ->
  PropertyTestData
mkPropertyTestData project propertyTest result = do
  let getDep = (`findBindingNameForExprHash` project)
  let depMap = mconcat (getDep <$> S.toList (ptDeps propertyTest))
  let failures = case result of
        PropertyTestSuccess -> mempty
        PropertyTestFailures es -> prettyPrint <$> S.toList es
  PropertyTestData
    (coerce $ ptName propertyTest)
    failures
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

data ExpressionData = ExpressionData
  { edHash :: Text,
    edPretty :: Text,
    edType :: Text,
    edBindings :: Map Name Text,
    edTypeBindings :: Map TyCon Text,
    edUnitTests :: [UnitTestData],
    edPropertyTests :: [PropertyTestData],
    edRuntimes :: Map RuntimeName RuntimeData,
    edGraphviz :: Text,
    edSourceItems :: [SourceItem],
    edInput :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

makeExpressionData ::
  Project Annotation ->
  StoreExpression Annotation ->
  Expr Name MonoType ->
  [Graphviz] ->
  Text ->
  ExpressionData
makeExpressionData project se typedExpr gv input =
  let mt = getTypeFromAnn typedExpr
      exprHash = getStoreExpressionHash se
      allTests = getTestsForExprHash project exprHash

      unitTests =
        mkUnitTestData project
          <$> M.elems
            ( M.mapMaybe
                filterUnitTest
                allTests
            )
      propertyTests =
        mkPropertyTestData project
          <$> M.elems (M.mapMaybe filterPropertyTest allTests)
            <*> pure PropertyTestSuccess
      matchingRuntimes =
        toRuntimeData
          <$> getValidRuntimes mt
   in ExpressionData
        (prettyPrint exprHash)
        (prettyPrint (storeExpression se))
        (prettyPrint mt)
        (prettyPrint <$> getBindings (storeBindings se))
        (prettyPrint <$> getTypeBindings (storeTypeBindings se))
        unitTests
        propertyTests
        matchingRuntimes
        (prettyGraphviz gv)
        (getExpressionSourceItems input typedExpr)
        input

expressionData ::
  StoreExpression Annotation ->
  Expr Name MonoType ->
  [Graphviz] ->
  Text ->
  ActionM ExpressionData
expressionData se typedExpr gv input = do
  project <- get
  pure $ makeExpressionData project se typedExpr gv input
