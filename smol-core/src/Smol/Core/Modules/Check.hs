{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Modules.Check
  ( checkModule,
  )
where

import Control.Monad.Except
import qualified Data.Text as T
import Smol.Core
import Smol.Core.Modules.FromParts
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Modules.Typecheck
import Smol.Core.Modules.Types.Module
import Smol.Core.Modules.Types.ModuleError
import Smol.Core.Modules.Types.ModuleItem

-- this is the front door as such
checkModule ::
  (MonadError (ModuleError Annotation) m) =>
  T.Text ->
  [ModuleItem Annotation] ->
  m (Module ResolvedDep (Type ResolvedDep Annotation))
checkModule input moduleItems = do
  (myModule, deps) <- moduleFromModuleParts moduleItems >>= resolveModuleDeps mempty
  typecheckModule input myModule deps
