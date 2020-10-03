module Language.Mimsa.Types.ForeignFunc where

import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.MonoType

type FFExpr a = Expr a Variable

data ForeignFunc a
  = NoArgs MonoType (IO (FFExpr a))
  | OneArg (MonoType, MonoType) (FFExpr a -> IO (FFExpr a))
