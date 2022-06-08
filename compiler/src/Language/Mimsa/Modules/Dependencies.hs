{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Modules.Dependencies (getValueDependencies, getModuleDeps) where

-- work out the dependencies between definitions inside a module

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Monad
import Language.Mimsa.Modules.Uses
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Modules.DefIdentifier
import Language.Mimsa.Types.Modules.Entity

filterDefs :: Set Entity -> Set DefIdentifier
filterDefs =
  S.fromList
    . mapMaybe
      ( \case
          EName name -> Just (DIName name)
          EInfix infixOp -> Just (DIInfix infixOp)
          _ -> Nothing
      )
    . S.toList

-- get the vars used by each def
-- explode if there's not available
getValueDependencies ::
  (Eq ann, Monoid ann) =>
  Module ann ->
  CheckM
    ( Map
        DefIdentifier
        ( Expr Name ann,
          Set DefIdentifier,
          Set Entity
        )
    )
getValueDependencies mod' = do
  traverse (getExprDependencies mod') (moExpressions mod')

getExprDependencies ::
  (Eq ann, Monoid ann) =>
  Module ann ->
  Expr Name ann ->
  CheckM (Expr Name ann, Set DefIdentifier, Set Entity)
getExprDependencies mod' expr =
  let allUses = extractUses expr
      nameDeps = filterDefs allUses
      unknownNameDeps =
        S.filter
          ( \dep ->
              S.notMember dep (M.keysSet (moExpressions mod'))
                && S.notMember dep (M.keysSet (moExpressionImports mod'))
          )
          nameDeps
   in if S.null unknownNameDeps
        then
          let localNameDeps =
                S.filter
                  ( `S.member`
                      M.keysSet (moExpressions mod')
                  )
                  nameDeps
           in pure (expr, localNameDeps, allUses)
        else throwError (ModuleErr (CannotFindValues unknownNameDeps))

-- starting at a root module,
-- create a map of each expr hash along with the modules it needs
-- so that we can typecheck them all
getModuleDeps ::
  (Show ann) =>
  Map ModuleHash (Module ann) ->
  Module ann ->
  CheckM
    ( Map
        ModuleHash
        ( Module ann,
          Set ModuleHash
        )
    )
getModuleDeps moduleDeps inputModule = do
  -- get this module's deps
  let deps =
        S.fromList
          ( M.elems (moExpressionImports inputModule)
              <> M.elems (moNamedImports inputModule)
          )
      mHash = hashModule inputModule
  -- recursively fetch sub-deps
  depModules <- traverse (lookupModule moduleDeps) (S.toList deps)
  subDeps <- traverse (getModuleDeps moduleDeps) depModules

  pure $ M.singleton mHash (inputModule, deps) <> mconcat subDeps
