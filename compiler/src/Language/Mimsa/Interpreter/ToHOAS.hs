module Language.Mimsa.Interpreter.ToHOAS (toHOAS, fromHOAS) where

import Language.Mimsa.Types.AST.Identifier
import Language.Mimsa.Types.AST.Expr
import qualified Language.Mimsa.Types.AST.HOASExpr as HOAS
import Data.Bifunctor (second)

toHOAS :: (Eq var) => Expr var ann -> HOAS.HOASExpr var ann
toHOAS (MyVar ann modName a) = HOAS.MyVar ann modName a
toHOAS (MyLiteral ann lit) = HOAS.MyLiteral ann lit
toHOAS (MyAnnotation ann mt body) = HOAS.MyAnnotation ann mt (toHOAS body)
toHOAS (MyLet ann ident expr body) = 
  toHOAS (MyApp ann (MyLambda ann ident body) expr)
toHOAS (MyLetPattern ann pat expr body) = HOAS.MyLetPattern ann pat (toHOAS expr) (toHOAS body)
toHOAS (MyInfix ann op a b) = HOAS.MyInfix ann op (toHOAS a) (toHOAS b)
toHOAS (MyIf ann a b c) = HOAS.MyIf ann (toHOAS a) (toHOAS b) (toHOAS c)
toHOAS (MyTuple ann a as) = HOAS.MyTuple ann (toHOAS a) (toHOAS <$> as)
toHOAS (MyLambda ann (Identifier iAnn ident) body) =
  HOAS.MyLambda ann (Identifier iAnn ident) (\arg ->
       replaceVars ident arg (toHOAS body)
     )
toHOAS (MyApp ann fn arg) = HOAS.MyApp ann (toHOAS fn) (toHOAS arg)
toHOAS (MyRecord ann as) = HOAS.MyRecord ann (toHOAS <$> as)
toHOAS (MyRecordAccess ann a name ) = HOAS.MyRecordAccess ann (toHOAS a) name
toHOAS (MyArray ann as) = HOAS.MyArray ann (toHOAS <$> as)
toHOAS (MyConstructor ann modName con) = HOAS.MyConstructor ann modName con
toHOAS (MyPatternMatch ann expr pats) = HOAS.MyPatternMatch ann (toHOAS expr) (second toHOAS <$> pats)
toHOAS (MyTypedHole ann a) = HOAS.MyTypedHole ann a

--
replaceVars :: (Eq var) =>
  var -> HOAS.HOASExpr var ann -> HOAS.HOASExpr var ann -> HOAS.HOASExpr var ann
replaceVars ident value =
  replaceInner
    where
      replaceInner (HOAS.MyVar _ Nothing identifier) | identifier == ident = value
      replaceInner (HOAS.MyVar ann modName identifier) = HOAS.MyVar ann modName identifier
      replaceInner other = mapHOASExpr replaceInner other


-- | Map a function `f` over the expression. This function takes care of
-- recursing through the Expression
mapHOASExpr :: (HOAS.HOASExpr a b -> HOAS.HOASExpr a b) -> HOAS.HOASExpr a b -> HOAS.HOASExpr a b
mapHOASExpr _ (HOAS.MyLiteral ann a) = HOAS.MyLiteral ann a
mapHOASExpr _ (HOAS.MyVar ann modName a) = HOAS.MyVar ann modName a
mapHOASExpr f (HOAS.MyAnnotation ann mt expr) =
  HOAS.MyAnnotation ann mt (f expr)
mapHOASExpr f (HOAS.MyLetPattern ann pat expr body) =
  HOAS.MyLetPattern ann pat (f expr) (f body)
mapHOASExpr f (HOAS.MyInfix ann op a b) = HOAS.MyInfix ann op (f a) (f b)
mapHOASExpr f (HOAS.MyLambda ann binder fn) = HOAS.MyLambda ann binder (f . fn)
mapHOASExpr f (HOAS.MyApp ann func arg) = HOAS.MyApp ann (f func) (f arg)
mapHOASExpr f (HOAS.MyIf ann matchExpr thenExpr elseExpr) =
  HOAS.MyIf ann (f matchExpr) (f thenExpr) (f elseExpr)
mapHOASExpr f (HOAS.MyTuple ann a as) = HOAS.MyTuple ann (f a) (f <$> as)
mapHOASExpr f (HOAS.MyRecord ann items) = HOAS.MyRecord ann (f <$> items)
mapHOASExpr f (HOAS.MyRecordAccess ann expr name) =
  HOAS.MyRecordAccess ann (f expr) name
mapHOASExpr f (HOAS.MyArray ann items) = HOAS.MyArray ann (f <$> items)
mapHOASExpr _ (HOAS.MyConstructor ann modName cons) = HOAS.MyConstructor ann modName cons
mapHOASExpr f (HOAS.MyPatternMatch ann matchExpr patterns) =
  HOAS.MyPatternMatch ann (f matchExpr) (second f <$> patterns)
mapHOASExpr _ (HOAS.MyTypedHole ann a) = HOAS.MyTypedHole ann a


fromHOAS :: HOAS.HOASExpr var ann -> Expr var ann
fromHOAS (HOAS.MyVar ann modName a) = MyVar ann modName a
fromHOAS (HOAS.MyLiteral ann lit) = MyLiteral ann lit
fromHOAS (HOAS.MyAnnotation ann mt body) = MyAnnotation ann mt (fromHOAS body)
fromHOAS (HOAS.MyLetPattern ann pat expr body) = MyLetPattern ann pat (fromHOAS expr) (fromHOAS body)
fromHOAS (HOAS.MyInfix ann op a b) = MyInfix ann op (fromHOAS a) (fromHOAS b)
fromHOAS (HOAS.MyIf ann a b c) = MyIf ann (fromHOAS a) (fromHOAS b) (fromHOAS c)
fromHOAS (HOAS.MyTuple ann a as) = MyTuple ann (fromHOAS a) (fromHOAS <$> as)
fromHOAS (HOAS.MyLambda ann (Identifier iAnn ident) f) =
  MyLambda ann (Identifier iAnn ident) (fromHOAS $ f (HOAS.MyVar ann Nothing ident))
fromHOAS (HOAS.MyApp ann fn arg) = MyApp ann (fromHOAS fn) (fromHOAS arg)
fromHOAS (HOAS.MyRecord ann as) = MyRecord ann (fromHOAS <$> as)
fromHOAS (HOAS.MyRecordAccess ann a name ) = MyRecordAccess ann (fromHOAS a) name
fromHOAS (HOAS.MyArray ann as) = MyArray ann (fromHOAS <$> as)
fromHOAS (HOAS.MyConstructor ann modName con) = MyConstructor ann modName con
fromHOAS (HOAS.MyPatternMatch ann expr pats) = MyPatternMatch ann (fromHOAS expr) (second fromHOAS <$> pats)
fromHOAS (HOAS.MyTypedHole ann a) = MyTypedHole ann a



