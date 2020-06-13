{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Types where

import Language.Mimsa.Types

data ReplAction
  = Help
  | Info Expr
  | Evaluate Expr
  | Bind Name Expr
  | ListBindings
