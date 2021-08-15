module Language.Mimsa.Repl.Types
  ( ReplAction (..),
  )
where

import Language.Mimsa.Backend.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Typechecker

data ReplAction ann
  = Help
  | Info (Expr Name ann)
  | Evaluate (Expr Name ann)
  | Tree (Expr Name ann)
  | Graph (Expr Name ann)
  | ProjectGraph
  | Bind Name (Expr Name ann)
  | OutputJS (Maybe Backend) (Expr Name ann)
  | TypeSearch MonoType
  | BindType (DataType ann)
  | Versions Name
  | ListBindings
  | AddUnitTest TestName (Expr Name ann)
  | ListTests (Maybe Name)
