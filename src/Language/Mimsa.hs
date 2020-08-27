module Language.Mimsa
  ( startInference,
    doInference,
    Expr (..),
    MonoType (..),
    Name,
    mkName,
    StringType (..),
    parseExpr,
    prettyPrint,
    repl,
  )
where

import Language.Mimsa.Repl (repl)
import Language.Mimsa.Syntax
import Language.Mimsa.Typechecker (doInference, startInference)
import Language.Mimsa.Types
