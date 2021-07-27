{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Language.Mimsa.Project.ReverseDeps where

import Data.Map (Map)
import Data.Set (Set)
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

newtype ReverseDeps = ReverseDeps
  { getReverseDeps ::
      Map ExprHash (Set ExprHash)
  }
  deriving newtype (Semigroup, Monoid)

getProjectReverseDeps :: Project ann -> ReverseDeps
getProjectReverseDeps _prj = mempty
