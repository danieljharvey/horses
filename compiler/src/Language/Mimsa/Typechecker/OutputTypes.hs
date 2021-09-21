module Language.Mimsa.Typechecker.OutputTypes (getExpressionSourceItems) where

import Data.Text (Text)
import Language.Mimsa.Printer
import Language.Mimsa.Project.SourceSpan
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project.SourceItem
import Language.Mimsa.Types.Typechecker

-- return types inside spans for server

getExpressionSourceItems :: Text -> Expr var TypedAnnotation -> [SourceItem]
getExpressionSourceItems input = foldExpr fn
  where
    fn ann =
      let monoType = fst ann
          sSpan =
            sourceSpan
              input
              (getAnnotationForType monoType)
       in case sSpan of
            Just sSpan' ->
              [SourceItem (prettyPrint monoType) sSpan']
            Nothing -> mempty

foldPattern :: (Monoid a) => (ann -> a) -> Pattern var ann -> a
foldPattern f (PVar ann _) = f ann
foldPattern f (PWildcard ann) = f ann
foldPattern f (PLit ann _) = f ann
foldPattern f (PConstructor ann _ as) = f ann <> foldMap (foldPattern f) as
foldPattern f (PPair ann a b) = f ann <> foldPattern f a <> foldPattern f b
foldPattern f (PRecord ann as) = f ann <> foldMap (foldPattern f) as
foldPattern f (PArray ann as spread) = f ann <> foldMap (foldPattern f) as <> foldSpread f spread
foldPattern f (PString ann _ _) = f ann

foldSpread :: (Monoid a) => (ann -> a) -> Spread var ann -> a
foldSpread _ NoSpread = mempty
foldSpread f (SpreadWildcard ann) = f ann
foldSpread f (SpreadValue ann _) = f ann

-- fold a function through all annotations in an expression and attached
foldExpr :: (Monoid a) => (ann -> a) -> Expr var ann -> a
foldExpr f (MyLiteral ann _) = f ann
foldExpr f (MyVar ann _) = f ann
foldExpr f (MyLet ann _ expr body) =
  f ann <> foldExpr f expr <> foldExpr f body
foldExpr f (MyPatternMatch ann expr pats) =
  f ann <> foldMap (foldPattern f) (fst <$> pats)
    <> foldMap (foldExpr f) (snd <$> pats)
    <> foldExpr f expr
foldExpr f (MyLetPattern ann pat expr body) =
  f ann <> foldPattern f pat <> foldExpr f expr <> foldExpr f body
foldExpr f (MyInfix ann _ a b) =
  f ann <> foldExpr f a <> foldExpr f b
foldExpr f (MyLambda ann _ body) =
  f ann <> foldExpr f body
foldExpr f (MyApp ann fn arg) =
  f ann <> foldExpr f fn <> foldExpr f arg
foldExpr f (MyIf ann predExpr thenExpr elseExpr) =
  f ann <> foldExpr f predExpr <> foldExpr f thenExpr <> foldExpr f elseExpr
foldExpr f (MyPair ann a b) = f ann <> foldExpr f a <> foldExpr f b
foldExpr f (MyRecord ann as) = f ann <> foldMap (foldExpr f) as
foldExpr f (MyRecordAccess ann record _) =
  f ann <> foldExpr f record
foldExpr f (MyArray ann as) =
  f ann <> foldMap (foldExpr f) as
foldExpr f (MyDefineInfix ann _ expr body) =
  f ann <> foldExpr f expr <> foldExpr f body
foldExpr f (MyData ann _ body) =
  f ann <> foldExpr f body
foldExpr f (MyConstructor ann _) = f ann
foldExpr f (MyTypedHole ann _) = f ann
