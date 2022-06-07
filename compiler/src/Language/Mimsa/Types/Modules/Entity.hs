{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Modules.Entity where

-- a thing
-- terrible, pls improve
import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.InfixOp
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.ModuleName

data Entity = 
  -- | a variable, `dog` 
  EName Name | 
    -- | an infix operator, `<|>`
    EInfix InfixOp | 
      -- | a namespaced var, `Prelude.id`
      ENamespacedName ModuleName Name | 
      -- | a typename, `Maybe`
    EType TypeName |
      -- | a namespaced typename, `Prelude.Either`
    ENamespacedType ModuleName TypeName  |
      -- | a constructor, `Just`
      EConstructor TyCon | 
        
-- | a namespaced constructor, `Maybe.Just`
  ENamespacedConstructor ModuleName TyCon
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass
    ( JSON.ToJSON,
      JSON.ToJSONKey,
      JSON.FromJSON,
      JSON.FromJSONKey
    )

instance Printer Entity where
  prettyPrint (EName name) = prettyPrint name
  prettyPrint (EInfix infixOp) = prettyPrint infixOp
  prettyPrint (ENamespacedName modName name) =
    prettyPrint modName <> "." <> prettyPrint name
  prettyPrint (EType typeName) = prettyPrint typeName
  prettyPrint (ENamespacedType modName typeName) =
    prettyPrint modName <> "." <> prettyPrint typeName
  prettyPrint (EConstructor tyCon) =
     prettyPrint tyCon
  prettyPrint (ENamespacedConstructor modName tyCon)
    = prettyPrint modName <> "." <> prettyPrint tyCon
