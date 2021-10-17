{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.TypedHoles
  ( typedHolesCheck,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bitraversable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import Language.Mimsa.Project.TypeSearch
import Language.Mimsa.Typechecker.NormaliseTypes
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

typedHolesCheck ::
  (MonadReader Swaps m, MonadError TypeError m, MonadState TypecheckState m) =>
  Map Variable MonoType ->
  Substitutions ->
  m ()
typedHolesCheck typeMap subs = do
  holes <- getTypedHoles subs
  if M.null holes
    then pure ()
    else do
      typeMap' <- swapTypeMapNames typeMap
      throwError (TypedHoles (getTypedHoleSuggestions typeMap' <$> holes))

getTypedHoleSuggestions ::
  Map Name MonoType ->
  (MonoType, Map Name MonoType) ->
  (MonoType, Set Name)
getTypedHoleSuggestions typeMap (mt, localTypeMap) = (normaliseType mt, suggestGlobal <> suggestLocal)
  where
    suggestGlobal = M.keysSet $ typeSearch typeMap mt
    suggestLocal = M.keysSet $ typeSearch localTypeMap mt
