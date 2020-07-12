{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Types where

import Language.Mimsa.Types

data ReplAction
  = Help
  | Info (Expr Name)
  | Evaluate (Expr Name)
  | Tree (Expr Name)
  | Bind Name (Expr Name)
  | ListBindings
  | Watch
