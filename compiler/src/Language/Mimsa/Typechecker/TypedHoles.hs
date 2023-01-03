{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Typechecker.TypedHoles
  ( typedHolesCheck,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Language.Mimsa.Project.TypeSearch
import Language.Mimsa.Typechecker.NormaliseTypes
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.Error
import Language.Mimsa.Core
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Types.Typechecker.Substitutions

typedHolesCheck ::
  (MonadError TypeError m, MonadState TypecheckState m) =>
  Map Name MonoType ->
  Substitutions ->
  m ()
typedHolesCheck typeMap subs = do
  holes <- getTypedHoles subs
  if M.null holes
    then pure ()
    else throwError (TypedHoles (getTypedHoleSuggestions typeMap <$> holes))

getTypedHoleSuggestions ::
  Map Name MonoType ->
  (MonoType, Map Name MonoType) ->
  (MonoType, Set FoundPath)
getTypedHoleSuggestions typeMap (mt, localTypeMap) =
  (normaliseType mt, suggestGlobal <> suggestLocal)
  where
    suggestGlobal = M.keysSet $ typeSearch typeMap mt
    suggestLocal = M.keysSet $ typeSearch localTypeMap mt
