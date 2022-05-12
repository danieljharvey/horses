{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Typechecker.NumberVars
  ( addNumbersToStoreExpression,
    addNumbersToExpression,
    NumberedExpr,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.TypeError
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker.Unique

newtype SubsState var ann = SubsState
  { ssCounter :: Int
  }
  deriving newtype (Eq, Ord, Show)

newtype SubsEnv var ann = SubsEnv
  { seScope :: Map var Unique
  }
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

type App var ann =
  ExceptT
    (TypeErrorF var ann)
    ( ReaderT
        (SubsEnv var ann)
        (State (SubsState var ann))
    )

type NumberedExpr var ann = Expr (var, Unique) ann

addNumbersToStoreExpression ::
  (Show ann) =>
  StoreExpression ann ->
  Either (TypeErrorF Name ann) (NumberedExpr Name ann)
addNumbersToStoreExpression storeExpr =
  let action = do
        -- add dependencies to scope
        let varsFromDeps =
              mconcat $
                (\(name, hash) -> M.singleton name (Dependency hash))
                  <$> M.toList (getBindings (storeBindings storeExpr))
        -- evaluate rest of expression using these
        withLambda
          varsFromDeps
          (markImports (storeExpression storeExpr))
   in evalState
        ( runReaderT
            (runExceptT action)
            mempty
        )
        (SubsState 0)

addNumbersToExpression ::
  (Show ann) =>
  Set Name ->
  Expr Name ann ->
  Either (TypeErrorF Name ann) (NumberedExpr Name ann)
addNumbersToExpression locals expr =
  let action = do
        -- add dependencies to scope
        let varsFromDeps =
              mconcat $
                (\name -> M.singleton name Local)
                  <$> S.toList locals
        -- evaluate rest of expression using these
        withLambda
          varsFromDeps
          (markImports expr)
   in evalState
        ( runReaderT
            (runExceptT action)
            mempty
        )
        (SubsState 0)

varFromIdent :: Identifier var ann -> var
varFromIdent ident = case ident of
  Identifier _ v -> v

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
  pure (Unique unique)

lookupVar :: (Ord var) => var -> App var ann (Maybe Unique)
lookupVar var = asks (M.lookup var . seScope)

-- given a var, given it a fresh number unless we already have a number for it
getVar :: (Ord var) => ann -> var -> App var ann (var, Unique)
getVar ann var = do
  found <- lookupVar var
  case found of
    Just unique -> pure (var, unique)
    Nothing -> do
      scope <- asks (M.keysSet . seScope)
      throwError (NameNotFoundInScope ann scope var)

-- step through Expr, replacing vars with numbered variables
markImports ::
  (Ord var, Show var, Show ann) =>
  Expr var ann ->
  App var ann (NumberedExpr var ann)
markImports (MyVar ann var) =
  MyVar ann <$> getVar ann var
markImports (MyAnnotation ann mt expr) =
  MyAnnotation ann mt
    <$> markImports expr
markImports (MyLet ann ident expr body) = do
  let var = varFromIdent ident
  unique <- nextNum
  MyLet
    ann
    (markIdentImports ident unique)
    <$> withLambda (M.singleton var unique) (markImports expr) -- include var in case it is used recursively
    <*> withLambda (M.singleton var unique) (markImports body)
markImports (MyLetPattern ann pat expr body) = do
  vars <- freshVarsFromPattern pat
  MyLetPattern ann
    <$> withLambda vars (markPatternImports pat)
    <*> markImports expr
    <*> withLambda vars (markImports body)
markImports (MyLiteral ann lit) =
  pure (MyLiteral ann lit)
markImports (MyInfix ann op a b) =
  MyInfix ann op <$> markImports a <*> markImports b
markImports (MyLambda ann ident body) = do
  unique <- nextNum
  MyLambda
    ann
    (markIdentImports ident unique)
    <$> withLambda (M.singleton (varFromIdent ident) unique) (markImports body)
markImports (MyApp ann fn val) =
  MyApp ann <$> markImports fn <*> markImports val
markImports (MyIf ann predExpr thenExpr elseExpr) =
  MyIf ann
    <$> markImports predExpr
    <*> markImports thenExpr
    <*> markImports elseExpr
markImports (MyPair ann a b) =
  MyPair ann <$> markImports a <*> markImports b
markImports (MyRecord ann as) =
  MyRecord ann <$> traverse markImports as
markImports (MyRecordAccess ann recExpr name) =
  MyRecordAccess ann <$> markImports recExpr <*> pure name
markImports (MyArray ann as) =
  MyArray ann <$> traverse markImports as
markImports (MyDefineInfix ann op fn expr) =
  MyDefineInfix ann op <$> markImports fn <*> markImports expr
markImports (MyData ann dt expr) =
  MyData ann dt <$> markImports expr
markImports (MyConstructor ann const') =
  pure (MyConstructor ann const')
markImports (MyPatternMatch ann patExpr patterns) =
  let markPatterns (pat, pExpr) = do
        uniqueMap <- freshVarsFromPattern pat
        withLambda
          uniqueMap
          ( (,)
              <$> markPatternImports pat
              <*> markImports pExpr
          )
   in MyPatternMatch ann
        <$> markImports patExpr
        <*> traverse markPatterns patterns
markImports (MyTypedHole ann name) = do
  -- always a unique number for these
  unique <- nextNum
  pure (MyTypedHole ann (name, unique))

markPatternImports ::
  (Ord var, Show var, Show ann) =>
  Pattern var ann ->
  App var ann (Pattern (var, Unique) ann)
markPatternImports pat =
  case pat of
    (PVar ann from) ->
      PVar ann <$> getVar ann from
    (PWildcard ann) ->
      pure $ PWildcard ann
    (PLit ann l) -> pure $ PLit ann l
    (PConstructor ann c d) ->
      PConstructor ann c
        <$> traverse markPatternImports d
    (PPair ann a b) ->
      PPair
        ann
        <$> markPatternImports a
        <*> markPatternImports b
    (PRecord ann as) ->
      PRecord ann <$> traverse markPatternImports as
    (PArray ann as a) ->
      PArray
        ann
        <$> traverse markPatternImports as
        <*> markSpreadNameImports a
    (PString ann as a) ->
      PString
        ann
        <$> markStringPartImports as
        <*> markStringPartImports a

markSpreadNameImports ::
  (Ord var) =>
  Spread var ann ->
  App var ann (Spread (var, Unique) ann)
markSpreadNameImports (SpreadValue ann from') =
  SpreadValue ann <$> getVar ann from'
markSpreadNameImports (SpreadWildcard ann) = pure (SpreadWildcard ann)
markSpreadNameImports NoSpread = pure NoSpread

markStringPartImports ::
  (Ord var) =>
  StringPart var ann ->
  App var ann (StringPart (var, Unique) ann)
markStringPartImports (StrValue ann from') =
  StrValue ann <$> getVar ann from'
markStringPartImports (StrWildcard ann) = pure (StrWildcard ann)

markIdentImports ::
  Identifier var ann ->
  Unique ->
  Identifier (var, Unique) ann
markIdentImports (Identifier ann from') unique =
  Identifier ann (from', unique)
