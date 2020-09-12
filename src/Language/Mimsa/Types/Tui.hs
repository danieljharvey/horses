module Language.Mimsa.Types.Tui where

import qualified Brick.Widgets.List as L
import Data.List.NonEmpty (NonEmpty)
import Language.Mimsa.Types.Expr
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedDeps

data ExpressionInfo
  = ExpressionInfo
      { eiType :: MonoType,
        eiExpr :: Expr Name,
        eiName :: Name,
        eiDeps :: ResolvedDeps
      }

data TuiState
  = TuiState
      { project :: Project,
        uiState :: UIState
      }

newtype UIError
  = MissingStoreItems [Name]

data BindingsList
  = BindingsList
      { bName :: Name,
        bDeps :: ResolvedDeps,
        bList :: L.List () Name
      }

data UIState
  = TuiError UIError
  | ViewBindings (NonEmpty BindingsList)
