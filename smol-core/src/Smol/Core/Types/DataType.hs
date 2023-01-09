{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Types.DataType
  ( DataType (..),
  )
where

import Data.Map.Strict
import Smol.Core.Types.Constructor
import Smol.Core.Types.Identifier
import Smol.Core.Types.Type
import Smol.Core.Types.TypeName

data DataType ann = DataType
  { dtName :: TypeName,
    dtVars :: [Identifier],
    dtConstructors :: Map Constructor [Type ann]
  }
  deriving stock (Eq, Ord, Show)
