module Language.Mimsa.Types.ForeignFunc where

import Language.Mimsa.Types.AST
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Variable

type FFExpr = Expr Variable

data ForeignFunc
  = NoArgs MonoType (IO FFExpr)
  | OneArg (MonoType, MonoType) (FFExpr -> IO FFExpr)
  | TwoArgs (MonoType, MonoType, MonoType) (FFExpr -> FFExpr -> IO FFExpr)
  | ThreeArgs
      (MonoType, MonoType, MonoType, MonoType)
      (FFExpr -> FFExpr -> FFExpr -> IO FFExpr)
