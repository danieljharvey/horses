module Language.Mimsa.Types.Tui where

import qualified Brick.Widgets.List as L
import Data.List.NonEmpty (NonEmpty)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedDeps

data ExpressionInfo a
  = ExpressionInfo
      { eiType :: MonoType,
        eiExpr :: Expr a Name,
        eiName :: Name,
        eiDeps :: ResolvedDeps a
      }

data TuiState a
  = TuiState
      { project :: Project a,
        uiState :: UIState a
      }

newtype UIError
  = MissingStoreItems [Name]

data BindingsList a
  = BindingsList
      { bName :: Name,
        bDeps :: ResolvedDeps a,
        bList :: L.List () Name
      }

data UIState a
  = TuiError UIError
  | ViewBindings (NonEmpty (BindingsList a))
