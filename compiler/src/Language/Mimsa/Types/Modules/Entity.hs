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


data Entity = EName Name | EInfix InfixOp | ENamespacedName ModuleName Name
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

