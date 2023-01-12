{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}



module Server.Helpers.ExpressionData
  ( ExpressionData (..),
    UnitTestData (..),
    makeExpressionData,
    makeMinimalExpressionData,
  )
where

import Language.Mimsa.Project.SourceSpan


import qualified Data.Aeson as JSON
import Data.Bifunctor
import Data.Coerce
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.OpenApi hiding (get, Pattern)
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Core
import Language.Mimsa.Store
import Language.Mimsa.Typechecker.Elaborate
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


-- return types inside spans for server

getExpressionSourceItems :: Text -> Expr Name MonoType -> [SourceItem]
getExpressionSourceItems input = foldExpr fn
  where
    fn label monoType =
      let sSpan =
            sourceSpan
              input
              (getAnnotationForType monoType)
       in case sSpan of
            Just sSpan' ->
              [SourceItem (label <> " :: " <> prettyPrint monoType) sSpan']
            Nothing -> mempty

foldPattern :: (Monoid a) => (Text -> ann -> a) -> Pattern Name ann -> a
foldPattern fn pat =
  foldPattern' pat
  where
    f = fn (prettyPrint pat)
    foldPattern' (PVar ann _) = f ann
    foldPattern' (PWildcard ann) = f ann
    foldPattern' (PLit ann _) = f ann
    foldPattern' (PConstructor ann _ _ as) =
      f ann <> foldMap (foldPattern fn) as
    foldPattern' (PTuple ann a as) =
      f ann <> foldPattern fn a <> foldMap (foldPattern fn) as
    foldPattern' (PRecord ann as) =
      f ann <> foldMap (foldPattern fn) as
    foldPattern' (PArray ann as spread) =
      f ann <> foldMap (foldPattern fn) as <> foldSpread fn spread
    foldPattern' (PString ann _ _) =
      f ann

foldIdentifier :: (Text -> ann -> a) -> Identifier Name ann -> a
foldIdentifier fn ident =
  foldIdentifier' ident
  where
    f = fn (prettyPrint ident)
    foldIdentifier' (Identifier ann _) = f ann

foldSpread :: (Monoid a) => (Text -> ann -> a) -> Spread Name ann -> a
foldSpread fn spread =
  foldSpread' spread
  where
    f = fn (prettyPrint spread)
    foldSpread' NoSpread = mempty
    foldSpread' (SpreadWildcard ann) = f ann
    foldSpread' (SpreadValue ann _) = f ann

-- fold a function through all annotations in an expression and attached
foldExpr :: (Monoid a) => (Text -> ann -> a) -> Expr Name ann -> a
foldExpr fn expression =
  foldExpr' expression
  where
    f = fn (prettyPrint expression)
    foldExpr' (MyLiteral ann _) = f ann
    foldExpr' (MyAnnotation ann mt expr) =
      f ann <> foldExpr fn expr <> f (getAnnotationForType mt)
    foldExpr' (MyVar ann _ _) = f ann
    foldExpr' (MyLet ann binder expr body) =
      f ann
        <> foldIdentifier fn binder
        <> foldExpr fn expr
        <> foldExpr fn body
    foldExpr' (MyPatternMatch ann expr pats) =
      f ann
        <> foldMap (foldPattern fn . fst) pats
        <> foldMap (foldExpr fn . snd) pats
        <> foldExpr fn expr
    foldExpr' (MyLetPattern ann pat expr body) =
      f ann <> foldPattern fn pat <> foldExpr fn expr <> foldExpr fn body
    foldExpr' (MyInfix ann _ a b) =
      f ann <> foldExpr fn a <> foldExpr fn b
    foldExpr' (MyLambda ann ident body) =
      f ann <> foldIdentifier fn ident <> foldExpr fn body
    foldExpr' (MyApp ann func arg) =
      f ann <> foldExpr fn func <> foldExpr fn arg
    foldExpr' (MyIf ann predExpr thenExpr elseExpr) =
      f ann <> foldExpr fn predExpr <> foldExpr fn thenExpr <> foldExpr fn elseExpr
    foldExpr' (MyTuple ann a as) = f ann <> foldExpr fn a <> foldMap (foldExpr fn) as
    foldExpr' (MyRecord ann as) = f ann <> foldMap (foldExpr fn) as
    foldExpr' (MyTupleAccess ann tuple _) =
      f ann <> foldExpr fn tuple
    foldExpr' (MyRecordAccess ann record _) =
      f ann <> foldExpr fn record
    foldExpr' (MyArray ann as) =
      f ann <> foldMap (foldExpr fn) as
    foldExpr' (MyConstructor ann _ _) = f ann
    foldExpr' (MyTypedHole ann _) = f ann
