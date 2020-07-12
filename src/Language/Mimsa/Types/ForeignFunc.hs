module Language.Mimsa.Types.ForeignFunc where

import Language.Mimsa.Types.AST
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Name

type FFExpr = Expr Name

data ForeignFunc
  = NoArgs MonoType (IO FFExpr)
  | OneArg (MonoType, MonoType) (FFExpr -> IO FFExpr)
  | TwoArgs (MonoType, MonoType, MonoType) (FFExpr -> FFExpr -> IO FFExpr)
