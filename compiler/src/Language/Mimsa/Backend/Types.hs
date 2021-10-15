{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Backend.Types
  ( BackendM,
    Backend (..),
    Renderer (..),
  )
where

import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

type BackendM ann = Either (BackendError ann)

data Backend = CommonJS | ESModulesJS | Typescript
  deriving stock (Eq, Ord, Show)

data Renderer ann a = Renderer
  { renderFunc :: Name -> Expr Name ann -> BackendM ann a,
    renderImport :: (Name, ExprHash) -> BackendM ann a,
    renderStdLib :: BackendM ann a,
    renderExport :: Name -> BackendM ann a,
    renderTypeSignature :: MonoType -> BackendM ann a,
    renderNewline :: a
  }
