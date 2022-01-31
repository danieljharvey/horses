{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Helpers.ExpressionData
  ( ExpressionData (..),
    UnitTestData (..),
    makeExpressionData,
  )
where

import qualified Data.Aeson as JSON
import Data.Coerce
import Data.Map (Map)
import Data.OpenApi hiding (get)
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Transform.Warnings
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Typechecker.OutputTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project.SourceItem
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker
import Server.Helpers.TestData

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
    edRuntimes :: Map RuntimeName RuntimeData,
    edGraphviz :: Text,
    edSourceItems :: [SourceItem],
    edInput :: Text,
    edWarnings :: [Text]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

makeExpressionData ::
  StoreExpression Annotation ->
  Expr Name MonoType ->
  [Graphviz] ->
  Text ->
  ExpressionData
makeExpressionData se typedExpr gv input =
  let mt = getTypeFromAnn typedExpr
      exprHash = getStoreExpressionHash se

      matchingRuntimes =
        toRuntimeData
          <$> getValidRuntimes mt
   in ExpressionData
        (prettyPrint exprHash)
        (prettyPrint (storeExpression se))
        (prettyPrint mt)
        (prettyPrint <$> getBindings (storeBindings se))
        (prettyPrint <$> getTypeBindings (storeTypeBindings se))
        matchingRuntimes
        (prettyGraphviz gv)
        (getExpressionSourceItems input typedExpr)
        input
        (prettyPrint <$> getWarnings se)
