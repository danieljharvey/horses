{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.OutputTypes (getExpressionSourceItems) where

import Data.Text (Text)
import Language.Mimsa.Printer
import Language.Mimsa.Project.SourceSpan
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project.SourceItem
import Language.Mimsa.Types.Typechecker

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
    foldPattern' (PConstructor ann _ as) =
      f ann <> foldMap (foldPattern fn) as
    foldPattern' (PPair ann a b) =
      f ann <> foldPattern fn a <> foldPattern fn b
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
    foldExpr' (MyVar ann _) = f ann
    foldExpr' (MyLet ann binder expr body) =
      f ann <> foldIdentifier fn binder
        <> foldExpr fn expr
        <> foldExpr fn body
    foldExpr' (MyPatternMatch ann expr pats) =
      f ann <> foldMap (foldPattern fn . fst) pats
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
    foldExpr' (MyPair ann a b) = f ann <> foldExpr fn a <> foldExpr fn b
    foldExpr' (MyRecord ann as) = f ann <> foldMap (foldExpr fn) as
    foldExpr' (MyRecordAccess ann record _) =
      f ann <> foldExpr fn record
    foldExpr' (MyArray ann as) =
      f ann <> foldMap (foldExpr fn) as
    foldExpr' (MyDefineInfix ann _ expr body) =
      f ann <> foldExpr fn expr <> foldExpr fn body
    foldExpr' (MyData ann _ body) =
      f ann <> foldExpr fn body
    foldExpr' (MyConstructor ann _) = f ann
    foldExpr' (MyTypedHole ann _) = f ann
