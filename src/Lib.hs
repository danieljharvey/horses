{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startInference,
    doInference,
    Expr (..),
    MonoType (..),
    Name,
    mkName,
    UniVar (..),
    StringType (..),
    parseExpr,
    prettyPrint,
    repl,
  )
where

import Infer (doInference, startInference)
import Language
import Printer
import Repl (repl)
import Types
