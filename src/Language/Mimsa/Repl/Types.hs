{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Types where

import Language.Mimsa.Types

data ReplAction
  = Evaluate Expr
  | Bind Name Expr
  | ListBindings
