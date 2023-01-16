{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Types.Module.Entity where

-- a thing
-- terrible, pls improve
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import Smol.Core.Printer
import Smol.Core.Types.Module.ModuleName
import Smol.Core.Types.Identifier
import Smol.Core.Types.TypeName
import Smol.Core.Types.Constructor

data Entity
  = -- | a variable, `dog`
    EVar Identifier
      {-  | -- | an infix operator, `<|>`
    EInfix InfixOp
    -}
  | -- | a namespaced var, `Prelude.id`
    ENamespacedName ModuleName Identifier
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
    ( JSON.ToJSON,
      JSON.ToJSONKey,
      JSON.FromJSON,
      JSON.FromJSONKey
    )

instance Printer Entity where
  prettyDoc (EVar name) = prettyDoc name
  --prettyDoc (EInfix infixOp) = prettyDoc infixOp
  prettyDoc (ENamespacedName modName name) =
    prettyDoc modName <> "." <> prettyDoc name
  prettyDoc (EType typeName) = prettyDoc typeName
  prettyDoc (ENamespacedType modName typeName) =
    prettyDoc modName <> "." <> prettyDoc typeName
  prettyDoc (EConstructor tyCon) =
    prettyDoc tyCon
  prettyDoc (ENamespacedConstructor modName tyCon) =
    prettyDoc modName <> "." <> prettyDoc tyCon
