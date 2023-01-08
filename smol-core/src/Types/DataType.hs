{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Types.DataType
  ( DataType (..),
  )
where

import Data.Map.Strict
import Types.Constructor
import Types.Identifier
import Types.Type
import Types.TypeName

data DataType ann = DataType
  { dtName :: TypeName,
    dtVars :: [Identifier],
    dtConstructors :: Map Constructor [Type ann]
  }
  deriving stock (Eq, Ord, Show)
