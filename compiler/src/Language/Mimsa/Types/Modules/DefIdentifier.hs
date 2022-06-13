{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Modules.DefIdentifier
  ( DefIdentifier (..),
  )
where

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.InfixOp
import Language.Mimsa.Types.Identifiers

-- | different kinds of top-level definitions
data DefIdentifier = DIName Name | DIInfix InfixOp | DIType TypeName
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass
    ( JSON.ToJSON,
      JSON.ToJSONKey,
      JSON.FromJSON,
      JSON.FromJSONKey
    )

instance Printer DefIdentifier where
  prettyPrint (DIName name) = prettyPrint name
  prettyPrint (DIInfix infixOp) = prettyPrint infixOp
  prettyPrint (DIType typeName) = prettyPrint typeName
