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
  Map Name MonoType ->
  Substitutions ->
  MonoType ->
  m MonoType
typedHolesCheck typeMap subs mt = do
  holes <- getTypedHoles subs
  if M.null holes
    then pure mt
    else throwError (TypedHoles (getTypedHoleSuggestions typeMap <$> holes))

getTypedHoleSuggestions :: Map Name MonoType -> MonoType -> (MonoType, Set Name)
getTypedHoleSuggestions typeMap mt = (normaliseType mt, suggest)
  where
    suggest = M.keysSet $ typeSearch typeMap mt
