{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Typechecker.NumberVars (addNumbers, NumberedExpr, Unique) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store

newtype SubsState var ann = SubsState
  { ssCounter :: Unique
  }
  deriving newtype (Eq, Ord, Show)

newtype SubsEnv var ann = SubsEnv
  { seScope :: Map var Unique
  }
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

type App var ann = ReaderT (SubsEnv var ann) (State (SubsState var ann))

newtype Unique = Unique Int
  deriving newtype (Eq, Ord, Show, Num)

type NumberedExpr var ann = Expr var (ann, Maybe Unique)

addNumbers ::
  (Show ann) =>
  StoreExpression ann ->
  NumberedExpr Name ann
addNumbers storeExpr =
  evalState
    (runReaderT (markImports (storeExpression storeExpr)) mempty)
    (SubsState 0)

varFromIdent :: Identifier var ann -> var
varFromIdent ident = case ident of
  Identifier _ v -> v
  AnnotatedIdentifier _ v -> v

varsFromPattern :: (Ord var) => Pattern var ann -> Set var
varsFromPattern (PVar _ var) = S.singleton var
varsFromPattern (PWildcard _) = mempty
varsFromPattern (PLit _ _) = mempty
varsFromPattern (PConstructor _ _ as) = mconcat (varsFromPattern <$> as)
varsFromPattern (PPair _ a b) = varsFromPattern a <> varsFromPattern b
varsFromPattern (PRecord _ as) = mconcat (varsFromPattern <$> M.elems as)
varsFromPattern (PArray _ as spread) =
  let spreadVars = case spread of
        SpreadValue _ a -> S.singleton a
        SpreadWildcard _ -> mempty
        NoSpread -> mempty
   in spreadVars <> mconcat (varsFromPattern <$> as)
varsFromPattern (PString _ sHead sTail) =
  let stringPartVars sp = case sp of
        StrValue _ a -> S.singleton a
        StrWildcard _ -> mempty
   in stringPartVars sHead <> stringPartVars sTail

freshVarsFromPattern :: (Ord var) => Pattern var ann -> App var ann (Map var Unique)
freshVarsFromPattern pat =
  M.fromList
    <$> traverse
      (\var -> (,) var <$> nextNum)
      (S.toList (varsFromPattern pat))

withLambda :: (Ord var) => Map var Unique -> App var ann a -> App var ann a
withLambda newVars =
  local (\env -> env {seScope = newVars <> seScope env})

nextNum :: App var ann Unique
nextNum = do
  unique <- gets ssCounter
  modify
    ( \s ->
        s
          { ssCounter = unique + 1
          }
    )
  pure unique

lookupVar :: (Ord var) => var -> App var ann (Maybe Unique)
lookupVar var = asks (M.lookup var . seScope)

-- given a var, given it a fresh number unless we already have a number for it
getVar :: (Ord var) => ann -> var -> App var ann (ann, Maybe Unique)
getVar ann var = do
  found <- lookupVar var
  case found of
    Just unique -> pure (ann, Just unique)
    Nothing ->
      pure (ann, Nothing)

-- step through Expr, replacing vars with numbered variables
markImports ::
  (Ord var, Show var, Show ann) =>
  Expr var ann ->
  App var ann (NumberedExpr var ann)
markImports (MyVar ann var) =
  MyVar <$> getVar ann var <*> pure var
markImports (MyLet ann ident expr body) = do
  let var = varFromIdent ident
  unique <- nextNum
  MyLet (ann, Just unique)
    <$> markIdentImports ident
    <*> markImports expr
    <*> withLambda (M.singleton var unique) (markImports body)
markImports (MyLetPattern ann pat expr body) = do
  vars <- freshVarsFromPattern pat
  MyLetPattern (ann, Nothing) <$> withLambda vars (markPatternImports pat)
    <*> markImports expr
    <*> withLambda vars (markImports body)
markImports (MyLiteral ann lit) =
  pure (MyLiteral (ann, Nothing) lit)
markImports (MyInfix ann op a b) =
  MyInfix (ann, Nothing) op <$> markImports a <*> markImports b
markImports (MyLambda ann ident body) = do
  unique <- nextNum
  MyLambda (ann, Just unique)
    <$> markIdentImports ident
    <*> withLambda (M.singleton (varFromIdent ident) unique) (markImports body)
markImports (MyApp ann fn val) =
  MyApp (ann, Nothing) <$> markImports fn <*> markImports val
markImports (MyIf ann predExpr thenExpr elseExpr) =
  MyIf (ann, Nothing) <$> markImports predExpr
    <*> markImports thenExpr
    <*> markImports elseExpr
markImports (MyPair ann a b) =
  MyPair (ann, Nothing) <$> markImports a <*> markImports b
markImports (MyRecord ann as) =
  MyRecord (ann, Nothing) <$> traverse markImports as
markImports (MyRecordAccess ann recExpr name) =
  MyRecordAccess (ann, Nothing) <$> markImports recExpr <*> pure name
markImports (MyArray ann as) =
  MyArray (ann, Nothing) <$> traverse markImports as
markImports (MyDefineInfix ann op fn expr) =
  MyDefineInfix (ann, Nothing) op <$> markImports fn <*> markImports expr
markImports (MyData ann dt expr) =
  MyData (ann, Nothing) dt <$> markImports expr
markImports (MyConstructor ann const') =
  pure (MyConstructor (ann, Nothing) const')
markImports (MyPatternMatch ann patExpr patterns) =
  let markPatterns (pat, pExpr) = do
        uniqueMap <- freshVarsFromPattern pat
        withLambda
          uniqueMap
          ( (,) <$> markPatternImports pat
              <*> markImports pExpr
          )
   in MyPatternMatch (ann, Nothing) <$> markImports patExpr
        <*> traverse markPatterns patterns
markImports (MyTypedHole ann name) = pure (MyTypedHole (ann, Nothing) name)

markPatternImports ::
  (Ord var, Show var, Show ann) =>
  Pattern var ann ->
  App var ann (Pattern var (ann, Maybe Unique))
markPatternImports pat =
  case pat of
    (PVar ann from) ->
      PVar <$> getVar ann from <*> pure from
    (PWildcard ann) ->
      pure $ PWildcard (ann, Nothing)
    (PLit ann l) -> pure $ PLit (ann, Nothing) l
    (PConstructor ann c d) ->
      PConstructor (ann, Nothing) c
        <$> traverse markPatternImports d
    (PPair ann a b) ->
      PPair
        (ann, Nothing)
        <$> markPatternImports a
        <*> markPatternImports b
    (PRecord ann as) ->
      PRecord (ann, Nothing) <$> traverse markPatternImports as
    (PArray ann as a) ->
      PArray
        (ann, Nothing)
        <$> traverse markPatternImports as
        <*> markSpreadNameImports a
    (PString ann as a) ->
      PString
        (ann, Nothing)
        <$> markStringPartImports as
        <*> markStringPartImports a

markSpreadNameImports ::
  (Ord var) =>
  Spread var ann ->
  App var ann (Spread var (ann, Maybe Unique))
markSpreadNameImports (SpreadValue ann from') =
  SpreadValue <$> getVar ann from' <*> pure from'
markSpreadNameImports (SpreadWildcard ann) = pure (SpreadWildcard (ann, Nothing))
markSpreadNameImports NoSpread = pure NoSpread

markStringPartImports ::
  (Ord var) =>
  StringPart var ann ->
  App var ann (StringPart var (ann, Maybe Unique))
markStringPartImports (StrValue ann from') =
  StrValue <$> getVar ann from' <*> pure from'
markStringPartImports (StrWildcard ann) = pure (StrWildcard (ann, Nothing))

markIdentImports ::
  Identifier var ann ->
  App var ann (Identifier var (ann, Maybe Unique))
markIdentImports (Identifier ann from') =
  pure $ Identifier (ann, Nothing) from'
markIdentImports (AnnotatedIdentifier mt from') =
  pure $ AnnotatedIdentifier ((,Nothing) <$> mt) from'
