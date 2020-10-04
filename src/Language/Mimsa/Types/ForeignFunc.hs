module Language.Mimsa.Types.ForeignFunc where

import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.MonoType

type FFExpr ann = Expr Variable ann

data ForeignFunc ann
  = NoArgs MonoType (IO (FFExpr ann))
  | OneArg (MonoType, MonoType) (FFExpr ann -> IO (FFExpr ann))
