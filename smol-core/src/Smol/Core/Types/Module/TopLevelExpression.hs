{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Smol.Core.Types.Module.TopLevelExpression
  ( TopLevelExpression(..)
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Prettyprinter
import Smol.Core.Printer
import Smol.Core.Types.Constructor
import Smol.Core.Types.DataType
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
import Smol.Core.Types.Module.DefIdentifier
import Smol.Core.Types.Module.ModuleHash
import Smol.Core.Types.Module.ModuleName
import Smol.Core.Types.ParseDep
import Smol.Core.Types.Type
import Smol.Core.Types.TypeName

-- a module is, broadly, one file
-- it defines some datatypes, infixes and definitions
-- and it probably exports one or more of those

-- a single expression of zero or more exprs and an optional type
data TopLevelExpression dep ann = TopLevelExpression
  { tleExpr :: Expr dep ann,
    tleType :: Maybe (Type dep ann)
  }
  deriving stock (Functor, Generic)

deriving stock instance
  ( Eq ann,
    Eq (dep Identifier),
    Eq (dep Constructor),
    Eq (dep TypeName)
  ) =>
  Eq (TopLevelExpression dep ann)

deriving stock instance
  ( Ord ann,
    Ord (dep Identifier),
    Ord (dep Constructor),
    Ord (dep TypeName)
  ) =>
  Ord (TopLevelExpression dep ann)

deriving stock instance
  ( Show ann,
    Show (dep Identifier),
    Show (dep Constructor),
    Show (dep TypeName)
  ) =>
  Show (TopLevelExpression dep ann)

deriving anyclass instance
  ( ToJSON ann,
    ToJSONKey (dep Identifier),
    ToJSON (dep Identifier),
    ToJSON (dep Constructor),
    ToJSON (dep TypeName)
  ) =>
  ToJSON (TopLevelExpression dep ann)

deriving anyclass instance
  ( ToJSON ann,
    ToJSON (dep Identifier),
    ToJSON (dep Constructor),
    ToJSON (dep TypeName),
    ToJSONKey ann,
    ToJSONKey (dep Identifier),
    ToJSONKey (dep Constructor),
    ToJSONKey (dep TypeName)
  ) =>
  ToJSONKey (TopLevelExpression dep ann)

deriving anyclass instance
  ( FromJSONKey (dep Identifier), Ord (dep Identifier), FromJSON ann,
    FromJSON (dep Identifier),
    FromJSON (dep Constructor),
    FromJSON (dep TypeName)
  ) =>
  FromJSON (TopLevelExpression dep ann)

