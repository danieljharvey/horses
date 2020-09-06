module Language.Mimsa
  ( startInference,
    doInference,
    Expr (..),
    MonoType (..),
    Name,
    mkName,
    StringType (..),
    parseExpr,
    repl,
  )
where

import Language.Mimsa.Parser
import Language.Mimsa.Repl (repl)
import Language.Mimsa.Typechecker (doInference, startInference)
import Language.Mimsa.Types
