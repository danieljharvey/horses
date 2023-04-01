{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Typechecker.Typecheck
  ( typecheck,
  )
where

import Language.Mimsa.Typechecker.Globals
import Control.Monad.Except
import Control.Monad.State (runState)
import Control.Monad.Writer.CPS
import Data.Map.Strict (Map)
import Language.Mimsa.Core
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Typechecker.Solve
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.TypedHoles
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Types.Typechecker.Substitutions
import Language.Mimsa.Types.Typechecker.Unique

runElabM ::
  TypecheckState ->
  ElabM a ->
  Either TypeError (TypecheckWriter, TypecheckState, a)
runElabM tcState value =
  case either' of
    ((Right a, tcWriter), newTcState) ->
      Right (tcWriter, newTcState, a)
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
        ( elabExpr,
          TypecheckWriter
            { tcwConstraints = constraints,
              tcwGlobals = globals
            }
          ) <-
          listen (elab env expr)
        subs <- solve constraints
        typedHolesCheck typeMap subs
        elabExprWithGlobals <- applyGlobal globals elabExpr
        pure (subs, constraints, elabExprWithGlobals)
  (_, _, (subs, constraints, tyExpr)) <- runElabM (defaultTcState env) tcAction
  let typedExpr = applySubst subs tyExpr
  pure (subs, constraints, typedExpr, getTypeFromAnn typedExpr)
