{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Modules.Interpret (interpretModule) where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import Smol.Core
import Smol.Core.Interpreter.Types.Stack
import Smol.Core.Modules.Types
import Smol.Core.Modules.Types.ModuleError

--- interpret an expression in the context of a module (for now, the `main`
-- function)
interpretModule ::
  (Show ann, Eq ann, MonadError (ModuleError ann) m) =>
  DefIdentifier ResolvedDep ->
  Module ResolvedDep ann ->
  m (Expr ResolvedDep ann)
interpretModule (DIName exprName) inputModule = do
  let expressions = addEmptyStackFrames . tleExpr <$> moExpressions inputModule

  let mainExpression = case M.lookup exprName expressions of
        Just expr -> expr
        Nothing -> error $ "could not find '" <> show exprName <> "'"

  let otherExpressions = M.mapKeys LocalDefinition (M.delete exprName expressions)

  interpretExpr <-
    modifyError ErrorInInterpreter
      . liftEither
      . interpret otherExpressions
      $ mainExpression

  -- return resolved value, with extra metadata mess removed
  pure (edAnnotation <$> interpretExpr)
interpretModule defIdent _inputModule = error $ "interpretModule " <> show defIdent

