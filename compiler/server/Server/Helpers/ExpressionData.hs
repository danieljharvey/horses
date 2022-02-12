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
import Data.Map (Map)
import Data.OpenApi hiding (get)
import Data.Text (Text)
import GHC.Generics
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

data ExpressionData = ExpressionData
  { edHash :: Text,
    edPretty :: Text,
    edType :: Text,
    edBindings :: Map Name Text,
    edTypeBindings :: Map TyCon Text,
    edGraphviz :: Text,
    edSourceItems :: [SourceItem],
    edInput :: Text,
    edWarnings :: [Text],
    edCanOptimise :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

makeExpressionData ::
  StoreExpression Annotation ->
  Expr Name MonoType ->
  [Graphviz] ->
  Text ->
  [Warning] ->
  Bool ->
  ExpressionData
makeExpressionData se typedExpr gv input warnings canOptimise =
  let mt = getTypeFromAnn typedExpr
      exprHash = getStoreExpressionHash se
   in ExpressionData
        (prettyPrint exprHash)
        (prettyPrint (storeExpression se))
        (prettyPrint mt)
        (prettyPrint <$> getBindings (storeBindings se))
        (prettyPrint <$> getTypeBindings (storeTypeBindings se))
        (prettyGraphviz gv)
        (getExpressionSourceItems input typedExpr)
        input
        (prettyPrint <$> warnings)
        canOptimise
