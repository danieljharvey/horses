{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Modules.Types.DefIdentifier
  ( DefIdentifier (..),
  )
where

import qualified Data.Aeson as JSON
import GHC.Generics
import Smol.Core.Modules.Types.TestName
import Smol.Core.Printer
import Smol.Core.Types.Identifier
import Smol.Core.Types.TypeName
import Smol.Core.Typecheck.Typeclass.Types

-- | different kinds of top-level definitions
data DefIdentifier
  = DIName Identifier
  | DIType TypeName
  | DITest TestName
  | DIInstance (Constraint ())
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass
    ( JSON.ToJSON,
      JSON.ToJSONKey,
      JSON.FromJSON,
      JSON.FromJSONKey
    )

instance Printer DefIdentifier where
  prettyDoc (DIName name) = prettyDoc name
  -- prettyDoc (DIInfix infixOp) = prettyDoc infixOp
  prettyDoc (DIType typeName) = prettyDoc typeName
  prettyDoc (DITest testName) = "\"" <> prettyDoc testName <> "\""
  prettyDoc (DIInstance constraint)
    = prettyDoc constraint
