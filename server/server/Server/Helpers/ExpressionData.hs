{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Helpers.ExpressionData
  ( ExpressionData (..),
    UnitTestData (..),
    makeExpressionData,
    makeMinimalExpressionData,
  )
where

import qualified Data.Aeson as JSON
import Data.Bifunctor
import Data.Coerce
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.OpenApi hiding (get)
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Core
import Language.Mimsa.Store
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Typechecker.OutputTypes
import Language.Mimsa.Types.Project.SourceItem
import Language.Mimsa.Types.Store
import Server.Helpers.TestData

data ExpressionData = ExpressionData
  { edHash :: Text,
    edPretty :: Text,
    edType :: Text,
    edBindings :: Map Name Text,
    edTypeBindings :: Map TyCon Text,
    edSourceItems :: [SourceItem],
    edInput :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

sanitiseBindings :: Map (Maybe ModuleName, Name) ExprHash -> Map Name Text
sanitiseBindings = M.fromList . fmap (bimap combineName prettyPrint) . M.toList
  where
    combineName (modName, name') = case modName of
      Just m -> coerce m <> "." <> name'
      _ -> name'

sanitiseTypeBindings :: Map (Maybe ModuleName, TyCon) ExprHash -> Map TyCon Text
sanitiseTypeBindings = M.fromList . fmap (bimap combineName prettyPrint) . M.toList
  where
    combineName (modName, name') = case modName of
      Just m -> coerce m <> "." <> name'
      _ -> name'

makeExpressionData ::
  StoreExpression Annotation ->
  Expr Name MonoType ->
  Text ->
  ExpressionData
makeExpressionData se typedExpr input =
  let mt = getTypeFromAnn typedExpr
      exprHash = getStoreExpressionHash se
   in ExpressionData
        (prettyPrint exprHash)
        (prettyPrint se)
        (prettyPrint mt)
        (sanitiseBindings (storeBindings se))
        (sanitiseTypeBindings (storeTypeBindings se))
        (getExpressionSourceItems input typedExpr)
        input

-- this returns a partial but working set of expressionData
-- that is compatible with the old one
-- it is used in modules - once these become the default this type
-- probably needs reviewing
makeMinimalExpressionData ::
  StoreExpression Annotation ->
  MonoType ->
  Text ->
  ExpressionData
makeMinimalExpressionData se mt input =
  let exprHash = getStoreExpressionHash se
   in ExpressionData
        (prettyPrint exprHash)
        (prettyPrint se)
        (prettyPrint mt)
        (sanitiseBindings (storeBindings se))
        (sanitiseTypeBindings (storeTypeBindings se))
        mempty
        input
