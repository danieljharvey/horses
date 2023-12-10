{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Modules.Types.Entity where

-- a thing
-- terrible, pls improve
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import GHC.Generics (Generic)
import Smol.Core.Types.Constructor
import Smol.Core.Types.Identifier
import Smol.Core.Types.ModuleName
import Smol.Core.Types.TypeName
import qualified Prettyprinter as PP

data Entity
  = -- | a variable, `dog`
    EVar Identifier
  | {-  | -- | an infix operator, `<|>`
    EInfix InfixOp
    -}

    -- | a namespaced var, `Prelude.id`
    ENamespacedVar ModuleName Identifier
  | -- | a typename, `Maybe`
    EType TypeName
  | -- | a namespaced typename, `Prelude.Either`
    ENamespacedType ModuleName TypeName
  | -- | a constructor, `Just`
    EConstructor Constructor
  | -- \| a namespaced constructor, `Maybe.Just`
    ENamespacedConstructor ModuleName Constructor
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass
    ( ToJSON,
      ToJSONKey,
      FromJSON,
      FromJSONKey
    )

instance PP.Pretty Entity where
  pretty (EVar name) = PP.pretty name
  pretty (ENamespacedVar modName name) =
    PP.pretty modName <> "." <> PP.pretty name
  pretty (EType typeName) = PP.pretty typeName
  pretty (ENamespacedType modName typeName) =
    PP.pretty modName <> "." <> PP.pretty typeName
  pretty (EConstructor tyCon) =
    PP.pretty tyCon
  pretty (ENamespacedConstructor modName tyCon) =
    PP.pretty modName <> "." <> PP.pretty tyCon
