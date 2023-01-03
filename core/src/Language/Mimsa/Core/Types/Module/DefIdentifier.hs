{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Core.Types.Module.DefIdentifier
  ( DefIdentifier (..),
  )
where

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Core.Printer
import Language.Mimsa.Core.Types.AST.InfixOp
import Language.Mimsa.Core.Types.Identifiers
import Language.Mimsa.Core.Types.Tests

-- | different kinds of top-level definitions
data DefIdentifier
  = DIName Name
  | DIInfix InfixOp
  | DIType TypeName
  | DITest TestName
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
  prettyPrint (DITest testName) = "\"" <> prettyPrint testName <> "\""
