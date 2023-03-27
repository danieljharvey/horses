module Language.Mimsa.Interpreter.SimpleExpr (simpleExpr) where

import Data.Bifunctor
import Data.Functor
import Language.Mimsa.Core

-- | simpleExpr
simpleExpr :: Expr var ann -> Expr var ()
simpleExpr (MyConstructor _ _ tyCon) =
  MyConstructor mempty Nothing tyCon
simpleExpr (MyVar _ _ var) =
  MyVar mempty Nothing var
simpleExpr (MyLetPattern _ pat expr body) =
  MyLetPattern mempty (simplePattern pat) (simpleExpr expr) (simpleExpr body)
simpleExpr (MyLiteral _ lit) = MyLiteral mempty lit
simpleExpr (MyAnnotation _ mt expr) =
  MyAnnotation mempty (mt $> mempty) (simpleExpr expr)
simpleExpr (MyLet _ ident expr body) =
  MyLet mempty (ident $> mempty) (simpleExpr expr) (simpleExpr body)
simpleExpr (MyInfix _ op a b) = MyInfix mempty op (simpleExpr a) (simpleExpr b)
simpleExpr (MyLambda _ ident body) = MyLambda mempty (ident $> mempty) (simpleExpr body)
simpleExpr (MyApp _ fn val) = MyApp mempty (simpleExpr fn) (simpleExpr val)
simpleExpr (MyIf _ predExpr thenExpr elseExpr) =
  MyIf mempty (simpleExpr predExpr) (simpleExpr thenExpr) (simpleExpr elseExpr)
simpleExpr (MyTuple _ a as) = MyTuple mempty (simpleExpr a) (simpleExpr <$> as)
simpleExpr (MyRecord _ as) = MyRecord mempty (simpleExpr <$> as)
simpleExpr (MyRecordAccess _ expr name) = MyRecordAccess mempty (simpleExpr expr) name
simpleExpr (MyTupleAccess _ expr index) = MyTupleAccess mempty (simpleExpr expr) index
simpleExpr (MyArray _ as) = MyArray mempty (simpleExpr <$> as)
simpleExpr (MyPatternMatch _ expr pats) =
  MyPatternMatch mempty (simpleExpr expr) (bimap simplePattern simpleExpr <$> pats)
simpleExpr (MyTypedHole _ var) = MyTypedHole mempty var
simpleExpr (MyGlobal _ glob) = MyGlobal mempty glob

simplePattern :: Pattern var ann -> Pattern var ()
simplePattern (PVar _ var) = PVar mempty var
simplePattern (PConstructor _ _ tyCon args) =
  PConstructor mempty Nothing tyCon (simplePattern <$> args)
simplePattern (PWildcard _) = PWildcard mempty
simplePattern (PTuple _ a as) = PTuple mempty (simplePattern a) (simplePattern <$> as)
simplePattern (PRecord _ as) = PRecord mempty (simplePattern <$> as)
simplePattern (PLit _ lit) = PLit mempty lit
simplePattern (PArray _ vals spread) =
  let simpleSpread = case spread of
        SpreadValue _ a -> SpreadValue mempty a
        SpreadWildcard _ -> SpreadWildcard mempty
        NoSpread -> NoSpread
   in PArray mempty (simplePattern <$> vals) simpleSpread
simplePattern (PString _ pHead pTail) =
  let simplePart s = case s of
        StrValue _ a -> StrValue mempty a
        StrWildcard _ -> StrWildcard mempty
   in PString mempty (simplePart pHead) (simplePart pTail)
