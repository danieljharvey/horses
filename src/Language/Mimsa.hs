{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa
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

import Language.Mimsa.Infer (doInference, startInference)
import Language.Mimsa.Language
import Language.Mimsa.Printer
import Language.Mimsa.Repl (repl)
import Language.Mimsa.Types
