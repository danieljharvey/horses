module Language.Mimsa.Types.ForeignFunc where

import Language.Mimsa.Types.Expr
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Variable

type FFExpr = Expr Variable

data ForeignFunc
  = NoArgs MonoType (IO FFExpr)
  | OneArg (MonoType, MonoType) (FFExpr -> IO FFExpr)
