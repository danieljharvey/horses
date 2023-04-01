{-# LANGUAGE NamedFieldPuns #-}
module Language.Mimsa.Typechecker.Globals
  ( applyGlobal,
  addGlobal
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Language.Mimsa.Core
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.Typechecker.Substitutions
import Language.Mimsa.Types.Typechecker.Unique
import Control.Monad.Writer

-- | get any globals from Writer and apply them
addGlobal :: ElabM (Expr (Name, Unique) MonoType) -> ElabM (Expr (Name, Unique) MonoType)
addGlobal elabM = do
  (elabExpr,TypecheckWriter {tcwGlobals}) <- listen elabM
  applyGlobal tcwGlobals elabExpr

applyGlobal :: Map Name MonoType -> Expr (Name, Unique) MonoType -> ElabM (Expr (Name, Unique) MonoType)
applyGlobal globals expr | M.null globals = pure expr
applyGlobal globals expr = do
  let tyAnn = getAnnotationForType (getAnnotation expr)
      tyGlobals = MTGlobals tyAnn (MTRecord tyAnn globals Nothing)
  newTy <- case getAnnotation expr of
    ty@(MTGlobals _ _ rest) -> do
      subs <- unify (tyGlobals rest) ty
      pure $ applySubst subs ty
    other -> pure (tyGlobals other)
  pure (mapOuterExprAnnotation (const newTy) expr)

