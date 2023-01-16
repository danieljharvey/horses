{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Smol.Core.Types.ResolvedDep
  (


    ResolvedDep (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import Smol.Core.Helpers
import Smol.Core.Printer
import Smol.Core.Types.Constructor
import Smol.Core.Types.Identifier
import Smol.Core.Types.Module.ModuleHash
import Smol.Core.Types.Module.ModuleName
import Smol.Core.Types.Pattern
import Smol.Core.Types.Prim
import Smol.Core.Types.Type

data ResolvedDep identifier
  = LocalDefinition
      { rdIdentifier :: identifier
      }
  | ModuleDefinition
      { rdIdentifier :: identifier,
        rdModuleHash :: ModuleHash
      }
  | UniqueDefinition
      { rdIdentifier :: identifier,
        rdUnique :: Int
      }
  deriving stock (Eq, Ord, Show, Generic)

