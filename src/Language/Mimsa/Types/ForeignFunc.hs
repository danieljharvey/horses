module Language.Mimsa.Types.ForeignFunc where

import Language.Mimsa.Types.AST
import Language.Mimsa.Types.MonoType

data ForeignFunc
  = NoArgs MonoType (IO Expr)
  | OneArg (MonoType, MonoType) (Expr -> IO Expr)
  | TwoArgs (MonoType, MonoType, MonoType) (Expr -> Expr -> IO Expr)
