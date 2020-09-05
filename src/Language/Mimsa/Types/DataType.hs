{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.DataType
  ( DataType (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import GHC.Generics
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.TypeName

-------

data DataType
  = DataType
      { dtName :: Construct,
        dtVars :: [Name],
        dtConstructors :: Map Construct [TypeName]
      }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)
