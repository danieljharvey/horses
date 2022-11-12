module Language.Mimsa.Interpreter.ToHOAS (toHOAS, fromHOAS) where

import Data.Bifunctor (second)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Types.AST.Expr
import qualified Language.Mimsa.Types.AST.HOASExpr as HOAS
import Language.Mimsa.Types.AST.Identifier
import Language.Mimsa.Types.AST.Pattern
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.ExprUtils

toHOAS :: (Ord x) => 
    Expr (Name, x) ann -> HOAS.HOASExpr (Name, x) ann
toHOAS (MyVar ann modName a) = HOAS.MyVar ann modName a
toHOAS (MyLiteral ann lit) = HOAS.MyLiteral ann lit
toHOAS (MyAnnotation ann mt body) = HOAS.MyAnnotation ann mt (toHOAS body)
toHOAS (MyLet ann ident expr body) =
  toHOAS (MyApp ann (MyLambda ann ident body) expr)
toHOAS (MyLetPattern ann pat expr body) =
  let (hoasPat, hoasExpr) = fromPat ann pat expr
   in HOAS.MyLetPattern ann hoasPat hoasExpr (toHOAS body)
toHOAS (MyInfix ann op a b) = HOAS.MyInfix ann op (toHOAS a) (toHOAS b)
toHOAS (MyIf ann a b c) = HOAS.MyIf ann (toHOAS a) (toHOAS b) (toHOAS c)
toHOAS (MyTuple ann a as) = HOAS.MyTuple ann (toHOAS a) (toHOAS <$> as)
toHOAS (MyLambda ann (Identifier iAnn ident) body) =
  HOAS.MyLambda
    ann
    (Identifier iAnn ident)
    ( \arg ->
        replaceVars ident arg (toHOAS body)
    )
toHOAS (MyApp ann fn arg) = HOAS.MyApp ann (toHOAS fn) (toHOAS arg)
toHOAS (MyRecord ann as) = HOAS.MyRecord ann (toHOAS <$> as)
toHOAS (MyRecordAccess ann a name) = HOAS.MyRecordAccess ann (toHOAS a) name
toHOAS (MyArray ann as) = HOAS.MyArray ann (toHOAS <$> as)
toHOAS (MyConstructor ann modName con) = HOAS.MyConstructor ann modName con
toHOAS (MyPatternMatch ann patExpr pats) =
  HOAS.MyPatternMatch ann (toHOAS patExpr) (uncurry (fromPat ann) <$> pats)
toHOAS (MyTypedHole ann a) = HOAS.MyTypedHole ann a

fromPat ::
  (Ord x) => 
  ann ->
  Pattern (Name, x) ann ->
  Expr (Name, x) ann ->
  ( Pattern (Name, x) ann,
    HOAS.HOASExpr (Name, x) ann -> HOAS.HOASExpr (Name, x) ann
  )
fromPat ann pat pExpr =
  let patFnExpr iInput =
        let eInput = fromHOAS iInput
            vars = patternVars pat
         in toHOAS
              ( foldr
                  ( \(ident,_x) totalExpr ->
                      swapOutVar ident (MyRecordAccess ann eInput ident) totalExpr
                  )
                  pExpr
                  vars
              )
   in (pat, patFnExpr)

toPat :: (Ord x) => ann ->
  Pattern (Name, x) ann ->
    (HOAS.HOASExpr (Name, x) ann -> HOAS.HOASExpr (Name,x) ann) ->
  ( Pattern (Name, x) ann,
    Expr (Name, x) ann 
  )
toPat ann pat pExpr =
        let vars = patternVars pat
            input = foldMap (\k -> M.singleton (fst k) (HOAS.MyVar ann Nothing k)) 
                                    (S.toList vars)
            runPattern = fromHOAS (pExpr (HOAS.MyRecord ann input))
         in (pat, foldr reduceRecords runPattern vars)


--
replaceVars ::
  (Eq var) =>
  var ->
  HOAS.HOASExpr var ann ->
  HOAS.HOASExpr var ann ->
  HOAS.HOASExpr var ann
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
  HOAS.MyLetPattern ann pat (f . expr) (f body)
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
  HOAS.MyPatternMatch ann (f matchExpr) (second (f .) <$> patterns)
mapHOASExpr _ (HOAS.MyTypedHole ann a) = HOAS.MyTypedHole ann a

fromHOAS :: (Ord x) => 
  HOAS.HOASExpr (Name, x) ann -> Expr (Name, x) ann
fromHOAS (HOAS.MyVar ann modName a) = MyVar ann modName a
fromHOAS (HOAS.MyLiteral ann lit) = MyLiteral ann lit
fromHOAS (HOAS.MyAnnotation ann mt body) = 
  MyAnnotation ann mt (fromHOAS body)
fromHOAS (HOAS.MyLetPattern ann pat expr body) =
  MyLetPattern ann pat (snd (toPat ann pat expr)) (fromHOAS body)
fromHOAS (HOAS.MyInfix ann op a b) = MyInfix ann op (fromHOAS a) (fromHOAS b)
fromHOAS (HOAS.MyIf ann a b c) = MyIf ann (fromHOAS a) (fromHOAS b) (fromHOAS c)
fromHOAS (HOAS.MyTuple ann a as) = MyTuple ann (fromHOAS a) (fromHOAS <$> as)
fromHOAS (HOAS.MyLambda ann (Identifier iAnn ident) f) =
  MyLambda ann (Identifier iAnn ident) (fromHOAS $ f (HOAS.MyVar ann Nothing ident))
fromHOAS (HOAS.MyApp ann (HOAS.MyLambda _ (Identifier iAnn ident) rest) expr) =
  MyLet ann (Identifier iAnn ident) (fromHOAS expr) (fromHOAS $ rest (HOAS.MyVar ann Nothing ident))
fromHOAS (HOAS.MyApp ann fn arg) = MyApp ann (fromHOAS fn) (fromHOAS arg)
fromHOAS (HOAS.MyRecord ann as) = MyRecord ann (fromHOAS <$> as)
fromHOAS (HOAS.MyRecordAccess ann a name) = MyRecordAccess ann (fromHOAS a) name
fromHOAS (HOAS.MyArray ann as) = MyArray ann (fromHOAS <$> as)
fromHOAS (HOAS.MyConstructor ann modName con) = MyConstructor ann modName con
fromHOAS (HOAS.MyPatternMatch ann expr pats) = 
  MyPatternMatch ann (fromHOAS expr) (uncurry (toPat ann) <$> pats)
fromHOAS (HOAS.MyTypedHole ann a) = MyTypedHole ann a


-------

swapOutVar ::
  Name ->
  Expr (Name, x) ann ->
  Expr (Name, x) ann ->
  Expr (Name, x) ann
swapOutVar matchIdent new =
  go
  where
    go (MyVar _ _ (ident,_)) | matchIdent == ident = new
    go other = mapExpr go other

-- we end up with lots of `{ varName: varName }.varName , this turns them back
-- into `varName`
reduceRecords :: (Name,x) -> Expr (Name, x) ann -> Expr (Name, x) ann
reduceRecords ident =
  go
  where
    go expr@(MyRecordAccess ann (MyRecord _ items) accessIdent)
      | fst ident == accessIdent =
          case M.lookup (fst ident) items of
            Just (MyVar _ Nothing var)
              | fst var == fst ident -> MyVar ann Nothing var
            _ -> expr
    go other = mapExpr go other

patternVars :: (Ord var) => Pattern var ann -> Set var
patternVars (PVar _ v) = S.singleton v
patternVars other = patternMonoid patternVars other
