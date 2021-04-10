{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter.SwapName
  ( swapName,
  )
where

import Language.Mimsa.ExprUtils
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

-- step through Expr, replacing vars with numbered variables
swapName :: Variable -> Variable -> Expr Variable ann -> App ann (Expr Variable ann)
swapName from to (MyVar ann from') =
  pure $
    if from == from'
      then MyVar ann to
      else MyVar ann from'
swapName from to expr =
  bindExpr (swapName from to) expr
