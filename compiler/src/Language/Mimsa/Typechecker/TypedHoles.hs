{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.TypedHoles
  ( typedHolesCheck,
  )
where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import Language.Mimsa.Project.TypeSearch
import Language.Mimsa.Typechecker.NormaliseTypes
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

typedHolesCheck ::
  Map Name MonoType ->
  Substitutions ->
  MonoType ->
  TcMonad (Substitutions, MonoType)
typedHolesCheck typeMap subs mt = do
  holes <- getTypedHoles subs
  if M.null holes
    then pure (subs, normaliseType mt)
    else throwError (TypedHoles (getTypedHoleSuggestions typeMap <$> holes))

getTypedHoleSuggestions :: Map Name MonoType -> MonoType -> (MonoType, Set Name)
getTypedHoleSuggestions typeMap mt = (normaliseType mt, suggest)
  where
    suggest = M.keysSet $ typeSearch typeMap mt
