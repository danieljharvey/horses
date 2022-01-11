{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Mimsa.Types.AST.Identifier (Identifier (..)) where

import qualified Data.Aeson as JSON
import Data.OpenApi (ToSchema)
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Typechecker.MonoType

data Identifier var ann
  = Identifier
      { idAnn :: ann,
        idVar :: var
      }
  | AnnotatedIdentifier
      { idType :: Type ann,
        idVar :: var
      }
  deriving stock (Eq, Ord, Show, Functor, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

deriving anyclass instance
  (ToSchema var, ToSchema ann, JSON.ToJSONKey var) =>
  ToSchema (Identifier var ann)

instance (Printer var) => Printer (Identifier var ann) where
  prettyDoc (Identifier _ var) = prettyDoc var
  prettyDoc (AnnotatedIdentifier mt var) =
    "(" <> prettyDoc var <> ": " <> prettyDoc mt <> ")"
