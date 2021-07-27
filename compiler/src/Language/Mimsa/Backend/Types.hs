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

type BackendM ann = Either (BackendError ann)

data Backend = CommonJS
  deriving stock (Eq, Ord, Show)

data Renderer ann a = Renderer
  { renderFunc :: Name -> Expr Name ann -> BackendM ann a,
    renderImport :: Backend -> (Name, ExprHash) -> BackendM ann a,
    renderStdLib :: Backend -> BackendM ann a,
    renderExport :: Backend -> Name -> BackendM ann a
  }
