module Language.Mimsa.Types.ForeignFunc where

import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.MonoType

type FFExpr = Expr Variable

data ForeignFunc
  = NoArgs MonoType (IO FFExpr)
  | OneArg (MonoType, MonoType) (FFExpr -> IO FFExpr)
