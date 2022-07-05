{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Helpers.ModuleData
  ( ModuleData (..),
    makeModuleData,
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi hiding (get)
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Modules.Pretty
import Language.Mimsa.Printer
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Typechecker

data ModuleData = ModuleData
  { mdModuleHash :: Text,
    mdModulePretty :: Text,
    mdModuleType :: Text,
    mdInput :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

makeModuleData ::
  Module MonoType ->
  Text ->
  ModuleData
makeModuleData typedModule input =
  let moduleHash = typedModule
   in ModuleData
        { mdModuleHash = prettyPrint moduleHash,
          mdModulePretty = prettyPrint typedModule,
          mdModuleType = renderWithWidth 40 (modulePretty typedModule),
          mdInput = input
        }
