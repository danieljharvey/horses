module Language.Mimsa.Backend.Types (Backend (..), Renderer (..)) where

import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store

data Backend
  = CommonJS

data Renderer ann a = Renderer
  { renderFunc :: Name -> Expr Name ann -> a,
    renderImport :: Backend -> (Name, ExprHash) -> a,
    renderStdLib :: Backend -> a,
    renderExport :: Backend -> Name -> a
  }
