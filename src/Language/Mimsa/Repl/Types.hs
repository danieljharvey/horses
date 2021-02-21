{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Types
  ( ReplAction (..),
  )
where

import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Typechecker

data ReplAction ann
  = Help
  | Info (Expr Name ann)
  | Evaluate (Expr Name ann)
  | Tree (Expr Name ann)
  | Bind Name (Expr Name ann)
  | OutputJS (Expr Name ann)
  | TypeSearch MonoType
  | BindType DataType
  | Versions Name
  | ListBindings
  | AddUnitTest TestName (Expr Name ann)
  | ListTests (Maybe Name)
