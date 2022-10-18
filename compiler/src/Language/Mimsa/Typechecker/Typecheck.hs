{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Typechecker.Typecheck
  ( typecheck,
  )
where

import Control.Monad.Except
import Control.Monad.State (State, runState)
import Control.Monad.Writer
import Data.Map.Strict (Map)
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Typechecker.Solve
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.TypedHoles
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Types.Typechecker.Substitutions

import Language.Mimsa.Types.Typechecker.Unique

type ElabM =
  ExceptT
    TypeError
    ( WriterT
        [Constraint]
        (State TypecheckState)
    )

runElabM ::
  TypecheckState ->
  ElabM a ->
  Either TypeError ([Constraint], TypecheckState, a)
runElabM tcState value =
  case either' of
    ((Right a, constraints), newTcState) ->
      Right (constraints, newTcState, a)
    ((Left e, _), _) -> Left e
  where
    either' =
      runState
        (runWriterT (runExceptT value))
        tcState

-- run inference, and substitute everything possible
typecheck ::
  Map Name MonoType ->
  Environment ->
  Expr (Name, Unique) Annotation ->
  Either
    TypeError
    ( Substitutions,
      [Constraint],
      Expr (Name, Unique) MonoType,
      MonoType
    )
typecheck typeMap env expr = do
  let tcAction = do
        (elabExpr, constraints) <- listen (elab env expr)
        subs <- solve constraints
        typedHolesCheck typeMap subs
        pure (subs, constraints, elabExpr)
  (_, _, (subs, constraints, tyExpr)) <- runElabM (defaultTcState env) tcAction
  let typedExpr = applySubst subs tyExpr
  pure (subs, constraints, typedExpr, getTypeFromAnn typedExpr)
