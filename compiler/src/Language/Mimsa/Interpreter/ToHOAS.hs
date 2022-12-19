{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Interpreter.ToHOAS (toHOAS, fromHOAS, replaceVars) where

import Data.Maybe
import Data.Bifunctor (second)
import qualified Data.List.NonEmpty as NE
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.ExprUtils
import Language.Mimsa.Types.AST.Expr
import qualified Language.Mimsa.Types.AST.HOASExpr as HOAS
import Language.Mimsa.Types.AST.Identifier
import Language.Mimsa.Types.AST.Literal
import Language.Mimsa.Types.AST.Pattern
import Language.Mimsa.Types.AST.Spread
import Language.Mimsa.Types.AST.Literal
import Language.Mimsa.Types.AST.StringPart
import Language.Mimsa.Types.Identifiers

import Data.Monoid

import Language.Mimsa.Types.AST.StringPart

-- does an expression contain itself, ie, is it a recursive function?
import Language.Mimsa.Types.AST.Literal

import Language.Mimsa.Types.AST.StringPart
import Language.Mimsa.Types.Identifiers

import Data.Monoid

-- does an expression contain itself, ie, is it a recursive function?
hasVar ::
  (Eq var) =>
  var ->
  Expr var ann ->
  Bool
hasVar var expr = getAny $ withMonoid f expr
  where
    f (MyVar _ Nothing varA) | varA == var = (False, Any True)
    f MyVar {} = (False, Any False)
    f _ = (True, mempty)

toHOAS ::
  (Ord var, Show ann, Show var) =>
  Expr var ann ->
  HOAS.HOASExpr var ann
toHOAS (MyVar ann modName a) = HOAS.MyVar ann modName a
toHOAS (MyLiteral ann lit) = HOAS.MyLiteral ann lit
toHOAS (MyAnnotation ann mt body) = HOAS.MyAnnotation ann mt (toHOAS body)
toHOAS (MyLet ann recIdent@(Identifier _ rIdent) (MyLambda lambdaAnn argIdent@(Identifier _ aIdent) lBody) rest)
  | hasVar rIdent lBody =
      let hoasLambda =
            HOAS.MyRecursiveLambda
              lambdaAnn
              argIdent
              recIdent
              ( \arg ->
                  replaceVars aIdent arg (toHOAS lBody)
              )
       in HOAS.MyApp ann (toHOAS (MyLambda ann recIdent rest)) hoasLambda
toHOAS (MyLet ann ident expr body) =
  toHOAS (MyApp ann (MyLambda ann ident body) expr)
toHOAS (MyLetPattern ann pat expr body) =
  let (hoasPat, hoasBody) = toHOASPat ann pat body
   in HOAS.MyLetPattern ann hoasPat (toHOAS expr) hoasBody
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
toHOAS (MyTupleAccess ann a index) = HOAS.MyTupleAccess ann (toHOAS a) index
toHOAS (MyArray ann as) = HOAS.MyArray ann (toHOAS <$> as)
toHOAS (MyConstructor ann modName con) = HOAS.MyConstructor ann modName con
toHOAS (MyPatternMatch ann patExpr pats) =
  HOAS.MyPatternMatch ann (toHOAS patExpr) (uncurry (toHOASPat ann) <$> pats)
toHOAS (MyTypedHole ann a) = HOAS.MyTypedHole ann a

toHOASPat ::
  (Ord var, Show ann, Show var) =>
  ann ->
  Pattern var ann ->
  Expr var ann ->
  ( Pattern var ann,
    HOAS.HOASExpr var ann -> HOAS.HOASExpr var ann
  )
toHOASPat ann pat pExpr =
  let patFnExpr iInput =
        let eInput = fromHOAS iInput
            vars = patternVars pat
            varsWithIndex = zip (S.toList vars) ([0 ..] :: [Int])
         in toHOAS $
              if S.size vars == 1
                then swapOutVar (fst (head varsWithIndex)) eInput pExpr -- no tuple, just a single var
                else
                  foldr
                    ( \(ident, i) totalExpr ->
                        swapOutVar ident (MyTupleAccess ann eInput (fromIntegral i + 1)) totalExpr
                    )
                    pExpr
                    varsWithIndex
   in (pat, patFnExpr)

fromHOASPat ::
  (Ord var) =>
  ann ->
  Pattern var ann ->
  (HOAS.HOASExpr var ann -> HOAS.HOASExpr var ann) ->
  ( Pattern var ann,
    Expr var ann
  )
fromHOASPat ann pat pExpr =
  let vars = patternVars pat
      varsWithIndex = zip (S.toList vars) ([0 ..] :: [Int])
      runPattern =
        if S.size vars > 0
          then
            let neVars = HOAS.MyVar ann Nothing <$> NE.fromList (S.toList vars)
             in case NE.uncons neVars of
                  (a, Nothing) -> fromHOAS (pExpr a)
                  (a, Just as) -> fromHOAS (pExpr (HOAS.MyTuple ann a as))
          else fromHOAS (pExpr (HOAS.MyLiteral ann (MyBool True)))
   in (pat, foldr reduceTuples runPattern varsWithIndex)

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
    replaceInner (HOAS.MyVar _ Nothing identifier)
      | identifier == ident = value
    replaceInner other = mapHOASExpr replaceInner other

-- | Map a function `f` over the expression. This function takes care of
-- recursing through the Expression
mapHOASExpr :: (HOAS.HOASExpr a b -> HOAS.HOASExpr a b) -> HOAS.HOASExpr a b -> HOAS.HOASExpr a b
mapHOASExpr _ (HOAS.MyLiteral ann a) = HOAS.MyLiteral ann a
mapHOASExpr _ (HOAS.MyVar ann modName a) = HOAS.MyVar ann modName a
mapHOASExpr f (HOAS.MyAnnotation ann mt expr) =
  HOAS.MyAnnotation ann mt (f expr)
mapHOASExpr f (HOAS.MyLetPattern ann pat expr body) =
  HOAS.MyLetPattern ann pat (f expr) (f . body)
mapHOASExpr f (HOAS.MyInfix ann op a b) = HOAS.MyInfix ann op (f a) (f b)
mapHOASExpr f (HOAS.MyLambda ann binder fn) = HOAS.MyLambda ann binder (f . fn)
mapHOASExpr f (HOAS.MyRecursiveLambda ann binder recBinder fn) = HOAS.MyRecursiveLambda ann binder recBinder (f . fn)
mapHOASExpr f (HOAS.MyApp ann func arg) = HOAS.MyApp ann (f func) (f arg)
mapHOASExpr f (HOAS.MyIf ann matchExpr thenExpr elseExpr) =
  HOAS.MyIf ann (f matchExpr) (f thenExpr) (f elseExpr)
mapHOASExpr f (HOAS.MyTuple ann a as) = HOAS.MyTuple ann (f a) (f <$> as)
mapHOASExpr f (HOAS.MyRecord ann items) = HOAS.MyRecord ann (f <$> items)
mapHOASExpr f (HOAS.MyRecordAccess ann expr name) =
  HOAS.MyRecordAccess ann (f expr) name
mapHOASExpr f (HOAS.MyTupleAccess ann expr index) =
  HOAS.MyTupleAccess ann (f expr) index
mapHOASExpr f (HOAS.MyArray ann items) = HOAS.MyArray ann (f <$> items)
mapHOASExpr _ (HOAS.MyConstructor ann modName cons) = HOAS.MyConstructor ann modName cons
mapHOASExpr f (HOAS.MyPatternMatch ann matchExpr patterns) =
  HOAS.MyPatternMatch ann (f matchExpr) (second (f .) <$> patterns)
mapHOASExpr _ (HOAS.MyTypedHole ann a) = HOAS.MyTypedHole ann a

fromHOAS ::
  (Ord var) =>
  HOAS.HOASExpr var ann ->
  Expr var ann
fromHOAS (HOAS.MyVar ann modName a) = MyVar ann modName a
fromHOAS (HOAS.MyLiteral ann lit) = MyLiteral ann lit
fromHOAS (HOAS.MyAnnotation ann mt body) =
  MyAnnotation ann mt (fromHOAS body)
fromHOAS (HOAS.MyLetPattern ann pat expr body) =
  MyLetPattern ann pat (fromHOAS expr) (snd (fromHOASPat ann pat body))
fromHOAS (HOAS.MyInfix ann op a b) = MyInfix ann op (fromHOAS a) (fromHOAS b)
fromHOAS (HOAS.MyIf ann a b c) = MyIf ann (fromHOAS a) (fromHOAS b) (fromHOAS c)
fromHOAS (HOAS.MyTuple ann a as) = MyTuple ann (fromHOAS a) (fromHOAS <$> as)
fromHOAS (HOAS.MyLambda ann (Identifier iAnn ident) f) =
  MyLambda ann (Identifier iAnn ident) (fromHOAS $ f (HOAS.MyVar ann Nothing ident))
fromHOAS (HOAS.MyApp ann (HOAS.MyLambda _ (Identifier iAnn ident) rest) expr) =
  MyLet ann (Identifier iAnn ident) (fromHOAS expr) (fromHOAS $ rest (HOAS.MyVar ann Nothing ident))
fromHOAS (HOAS.MyRecursiveLambda ann (Identifier iAnn ident) _ rest) =
  MyLambda
    ann
    (Identifier iAnn ident)
    (fromHOAS $ rest (HOAS.MyVar ann Nothing ident))
fromHOAS (HOAS.MyApp ann fn arg) = MyApp ann (fromHOAS fn) (fromHOAS arg)
fromHOAS (HOAS.MyRecord ann as) = MyRecord ann (fromHOAS <$> as)
fromHOAS (HOAS.MyRecordAccess ann a name) = MyRecordAccess ann (fromHOAS a) name
fromHOAS (HOAS.MyTupleAccess ann a index) = MyTupleAccess ann (fromHOAS a) index
fromHOAS (HOAS.MyArray ann as) = MyArray ann (fromHOAS <$> as)
fromHOAS (HOAS.MyConstructor ann modName con) = MyConstructor ann modName con
fromHOAS (HOAS.MyPatternMatch ann expr pats) =
  MyPatternMatch ann (fromHOAS expr) (uncurry (fromHOASPat ann) <$> pats)
fromHOAS (HOAS.MyTypedHole ann a) = MyTypedHole ann a

-------

swapOutVar ::
  (Eq var) =>
  var ->
  Expr var ann ->
  Expr var ann ->
  Expr var ann
swapOutVar matchIdent new =
  go
  where
    go (MyVar _ Nothing ident) | matchIdent == ident = new
    go other = mapExpr go other

-- | we have to wrap all the arguments for a pattern match into a tuple, this
-- removes all the waste and tidies up again
-- | given `('a", 1), turn any ("a",..).1 into "a"
-- | given ('b', 2), turn (_, "b",...).2 into "b"
reduceTuples :: (Eq var) => (var ,Int) -> Expr var ann -> Expr var ann
reduceTuples (ident,index) =
  go 
  where
    go expr@(MyTupleAccess ann (MyTuple _ a as) accessIdent) | 
        index + 1 == fromIntegral accessIdent =
      let allAs = [a] <> NE.toList as
       in case listToMaybe (drop index allAs) of
            Just (MyVar _ Nothing var)
              | var == ident -> MyVar ann Nothing var
            _ -> expr
    go other = mapExpr go other

  {-
reduceRecords :: (Name, x) -> Expr (Name, x) ann -> Expr (Name, x) ann
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
-}

patternVars :: (Ord var) => Pattern var ann -> Set var
patternVars (PVar _ v) = S.singleton v
patternVars (PString _ sHead sTail) =
  let fromStrPart = \case
        StrValue _ a -> S.singleton a
        StrWildcard _ -> mempty
   in fromStrPart sHead <> fromStrPart sTail
patternVars pArr@(PArray _ _ pTail) =
  let pSpread = case pTail of
        SpreadValue _ a -> S.singleton a
        SpreadWildcard _ -> mempty
        NoSpread -> mempty
   in patternMonoid patternVars pArr <> pSpread
patternVars other = patternMonoid patternVars other
