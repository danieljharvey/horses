{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smol.Core.Typecheck.Typeclass.Types
  ( Typeclass (..),
    Constraint (..),
    Instance (..),
    module Smol.Core.Typecheck.Typeclass.Types.TypeclassName,
  )
where

import Control.Monad.Identity
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import GHC.Generics (Generic)
import qualified Prettyprinter as PP
import Smol.Core.Printer
import Smol.Core.Typecheck.Typeclass.Types.TypeclassName
import Smol.Core.Types

-- | the typeclass described in it's most general form, ie
-- class Show a where show :: a -> String
data Typeclass ann = Typeclass
  { tcName :: TypeclassName,
    tcArgs :: [Identifier],
    tcFuncName :: Identifier,
    tcFuncType :: Type Identity ann
  }
  deriving stock (Eq, Ord, Show, Functor, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Constraint ann = Constraint
  { conTypeclass :: TypeclassName,
    conType :: [Type Identity ann]
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic)
  deriving anyclass (ToJSON, ToJSONKey, FromJSON, FromJSONKey)

instance Printer (Constraint ann) where
  prettyDoc (Constraint tcn tys) =
    prettyDoc tcn
      PP.<+> PP.concatWith
        (\a b -> a <> " " <> b)
        (prettyDoc <$> tys)

data Instance dep ann = Instance
  { inConstraints :: [Constraint ann],
    inExpr :: Expr dep ann
  }
  deriving stock (Functor, Generic)

deriving stock instance
  ( Eq ann,
    Eq (dep Constructor),
    Eq (dep TypeName),
    Eq (dep Identifier)
  ) =>
  Eq (Instance dep ann)

deriving stock instance
  ( Ord ann,
    Ord (dep Constructor),
    Ord (dep TypeName),
    Ord (dep Identifier)
  ) =>
  Ord (Instance dep ann)

deriving stock instance
  ( Show ann,
    Show (dep Constructor),
    Show (dep TypeName),
    Show (dep Identifier)
  ) =>
  Show (Instance dep ann)

deriving anyclass instance
  ( ToJSONKey (dep Identifier),
    ToJSON ann,
    ToJSON (dep Identifier),
    ToJSON (dep Constructor),
    ToJSON (dep TypeName)
  ) =>
  ToJSON (Instance dep ann)

deriving anyclass instance
  ( FromJSON ann,
    FromJSON (dep Constructor),
    FromJSON (dep Identifier),
    FromJSONKey (dep Identifier),
    Ord (dep Identifier),
    FromJSON (dep TypeName)
  ) =>
  FromJSON (Instance dep ann)

instance
  ( Printer (dep Constructor),
    Printer (dep TypeName),
    Printer (dep Identifier)
  ) =>
  Printer (Instance dep ann)
  where
  prettyDoc (Instance [] expr) = prettyDoc expr
  prettyDoc (Instance constraints expr) =
    "(" <> PP.concatWith (\a b -> a <> ", " <> b) (prettyDoc <$> constraints) <> ") => " <> prettyDoc expr
