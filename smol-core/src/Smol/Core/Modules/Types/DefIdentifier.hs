{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Smol.Core.Modules.Types.DefIdentifier
  ( DefIdentifier (..),
  )
where

import GHC.Generics (Generic)
import Smol.Core.Modules.Types.TestName
import Smol.Core.Printer
import Smol.Core.Typecheck.Typeclass.Types
import Smol.Core.Types.Constructor
import Smol.Core.Types.Identifier
import Smol.Core.Types.TypeName

-- | different kinds of top-level definitions
data DefIdentifier dep
  = DIName Identifier
  | DIType TypeName
  | DITest TestName
  | DIInstance (Constraint dep ())
  deriving stock (Generic)

deriving stock instance
  (Eq (dep Constructor), Eq (dep TypeName), Eq (dep Identifier)) =>
  Eq (DefIdentifier dep)

deriving stock instance
  (Ord (dep Constructor), Ord (dep TypeName), Ord (dep Identifier)) =>
  Ord (DefIdentifier dep)

deriving stock instance
  (Show (dep Constructor), Show (dep TypeName), Show (dep Identifier)) =>
  Show (DefIdentifier dep)

instance (Printer (dep Identifier), Printer (dep TypeName)) => Printer (DefIdentifier dep) where
  prettyDoc (DIName name) = prettyDoc name
  -- prettyDoc (DIInfix infixOp) = prettyDoc infixOp
  prettyDoc (DIType typeName) = prettyDoc typeName
  prettyDoc (DITest testName) = "\"" <> prettyDoc testName <> "\""
  prettyDoc (DIInstance constraint) =
    prettyDoc constraint
