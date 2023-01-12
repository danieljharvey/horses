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
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Core
import Language.Mimsa.Typechecker.Error.TypeError
-- import Language.Mimsa.Types.Store
import Language.Mimsa.Typechecker.Types.Unique

newtype SubsState var ann = SubsState
  { ssCounter :: Int
  }
  deriving newtype (Eq, Ord, Show)

newtype SubsEnv hash var ann = SubsEnv
  { seScope :: Map (var, Maybe ModuleName) (Unique hash)
  }
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

type App hash var ann =
  ExceptT
    (TypeErrorF var ann)
    ( ReaderT
        (SubsEnv hash var ann)
        (State (SubsState var ann))
    )

type NumberedExpr hash var ann = Expr (var, Unique hash) ann

addNumbersToStoreExpression ::
  (Show ann) =>
  Expr Name ann ->
  Map (Maybe ModuleName, Name) hash ->
  Either (TypeErrorF Name ann) (NumberedExpr hash Name ann)
addNumbersToStoreExpression expr bindings =
  let action = do
        -- add dependencies to scope
        let varsFromDeps =
              mconcat $
                ( \((modName, name), hash) ->
                    M.singleton (name, modName) (Dependency hash)
                )
                  <$> M.toList bindings
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

addNumbersToExpression ::
  (Show ann) =>
  Set Name ->
  Map Name hash ->
  Map ModuleName (ModuleHash, Set Name) ->
  Expr Name ann ->
  Either (TypeErrorF Name ann) (NumberedExpr hash Name ann)
addNumbersToExpression locals imports modules expr =
  let action = do
        -- add dependencies to scope
        let localVars =
              mconcat $
                (\name -> M.singleton (name, Nothing) Local)
                  <$> S.toList locals

        let importVars =
              mconcat $
                (\(name, hash) -> M.singleton (name, Nothing) (Dependency hash))
                  <$> M.toList imports
        let moduleVars =
              mconcat $
                ( \(modName, (hash, names)) ->
                    M.fromList
                      ( (\name -> ((name, Just modName), ModuleDep hash))
                          <$> S.toList names
                      )
                )
                  <$> M.toList modules

        -- evaluate rest of expression using these
        withLambda
          (localVars <> importVars <> moduleVars)
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
varsFromPattern (PConstructor _ _ _ as) = mconcat (varsFromPattern <$> as)
varsFromPattern (PTuple _ a as) = varsFromPattern a <> mconcat (varsFromPattern <$> NE.toList as)
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

freshVarsFromPattern ::
  (Ord var) =>
  Pattern var ann ->
  App hash var ann (Map (var, Maybe ModuleName) (Unique hash))
freshVarsFromPattern pat =
  M.fromList
    <$> traverse
      (\var -> (,) (var, Nothing) <$> nextNum)
      (S.toList (varsFromPattern pat))

withLambda :: (Ord var) =>
  Map (var, Maybe ModuleName) (Unique hash) -> App hash var ann a -> App hash var ann a
withLambda newVars =
  local (\env -> env {seScope = newVars <> seScope env})

nextNum :: App hash var ann (Unique hash)
nextNum = do
  unique <- gets ssCounter
  modify
    ( \s ->
        s
          { ssCounter = unique + 1
          }
    )
  pure (Unique unique)

lookupVar ::
  (Ord var) =>
  var ->
  Maybe ModuleName ->
  App hash var ann (Maybe (Unique hash))
lookupVar var maybeMod =
  asks (M.lookup (var, maybeMod) . seScope)

-- given a var, given it a fresh number unless we already have a number for it
getVar :: (Ord var) => ann -> var -> Maybe ModuleName ->
    App hash var ann (var, Unique hash)
getVar ann var maybeMod = do
  found <- lookupVar var maybeMod
  case found of
    Just unique -> pure (var, unique)
    Nothing -> do
      scope <- asks (M.keysSet . seScope)
      throwError (NameNotFoundInScope ann scope maybeMod var)

-- step through Expr, replacing vars with numbered variables
markImports ::
  (Ord var, Show var, Show ann) =>
  Expr var ann ->
  App hash var ann (NumberedExpr hash var ann)
markImports (MyVar ann modName var) =
  MyVar ann modName <$> getVar ann var modName
markImports (MyAnnotation ann mt expr) =
  MyAnnotation ann mt
    <$> markImports expr
markImports (MyLet ann ident expr body) = do
  let var = varFromIdent ident
  unique <- nextNum
  MyLet
    ann
    (markIdentImports ident unique)
    <$> withLambda
      (M.singleton (var, Nothing) unique)
      (markImports expr) -- include var in case it is used recursively
    <*> withLambda
      (M.singleton (var, Nothing) unique)
      (markImports body)
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
    <$> withLambda (M.singleton (varFromIdent ident, Nothing) unique) (markImports body)
markImports (MyApp ann fn val) =
  MyApp ann <$> markImports fn <*> markImports val
markImports (MyIf ann predExpr thenExpr elseExpr) =
  MyIf ann
    <$> markImports predExpr
    <*> markImports thenExpr
    <*> markImports elseExpr
markImports (MyTuple ann a as) =
  MyTuple ann <$> markImports a <*> traverse markImports as
markImports (MyRecord ann as) =
  MyRecord ann <$> traverse markImports as
markImports (MyRecordAccess ann recExpr name) =
  MyRecordAccess ann <$> markImports recExpr <*> pure name
markImports (MyTupleAccess ann tupleExpr index) =
  MyTupleAccess ann <$> markImports tupleExpr <*> pure index
markImports (MyArray ann as) =
  MyArray ann <$> traverse markImports as
markImports (MyConstructor ann modName const') =
  pure (MyConstructor ann modName const')
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
  App hash var ann (Pattern (var, Unique hash) ann)
markPatternImports pat =
  case pat of
    (PVar ann from) ->
      PVar ann <$> getVar ann from Nothing
    (PWildcard ann) ->
      pure $ PWildcard ann
    (PLit ann l) -> pure $ PLit ann l
    (PConstructor ann c d e) ->
      PConstructor ann c d
        <$> traverse markPatternImports e
    (PTuple ann a as) ->
      PTuple
        ann
        <$> markPatternImports a
        <*> traverse markPatternImports as
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
  App hash var ann (Spread (var, Unique hash) ann)
markSpreadNameImports (SpreadValue ann from') =
  SpreadValue ann <$> getVar ann from' Nothing
markSpreadNameImports (SpreadWildcard ann) = pure (SpreadWildcard ann)
markSpreadNameImports NoSpread = pure NoSpread

markStringPartImports ::
  (Ord var) =>
  StringPart var ann ->
  App hash var ann (StringPart (var, Unique hash) ann)
markStringPartImports (StrValue ann from') =
  StrValue ann <$> getVar ann from' Nothing
markStringPartImports (StrWildcard ann) = pure (StrWildcard ann)

markIdentImports ::
  Identifier var ann ->
  Unique hash ->
  Identifier (var, Unique hash) ann
markIdentImports (Identifier ann from') unique =
  Identifier ann (from', unique)
