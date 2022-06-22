{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ReplNew.Actions.Compile
  ( doOutputJS,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Compile as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Backend.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import ReplNew.Helpers
import ReplNew.ReplM

doOutputJS ::
  Project Annotation ->
  Text ->
  Maybe Backend ->
  Expr Name Annotation ->
  ReplM (Error Annotation) ()
doOutputJS project input maybeBackend expr = do
  let be = fromMaybe ESModulesJS maybeBackend
  (_, _, resolvedExpr) <-
    replMFromEither $
      Actions.run
        project
        (Actions.typecheckExpression project input expr)
  (_, (_, _)) <-
    toReplM project (Actions.compile be (reStoreExpression resolvedExpr))
  replOutput @Text "Compilation complete!"
