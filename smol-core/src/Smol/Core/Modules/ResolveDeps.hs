{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smol.Core.Modules.ResolveDeps
  (
    resolveExprDeps
  )
where

import Smol.Core

resolveExprDeps :: Expr ParseDep ann -> Expr ResolvedDep ann
resolveExprDeps _ = error "sdfsdf"

