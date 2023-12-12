{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Typecheck.Types.TCWrite (TCWrite (..), filterSubstitutions, filterTypeclassUses) where

import Data.Maybe (mapMaybe)
import Data.String
import Smol.Core.Printer
import Smol.Core.Types.Identifier
import Smol.Core.Types.ResolvedDep
import Smol.Core.Types.TypeclassName
import Smol.Typecheck.Types.Substitution

-- stuff emitted during typechecking
data TCWrite ann
  = TCWSubstitution
      (Substitution ResolvedDep ann)
  | TCWTypeclassUse
      (ResolvedDep Identifier)
      TypeclassName
      [(Identifier, Integer)]
  deriving stock (Eq, Ord, Show)

instance (Show ann) => Printer (TCWrite ann) where
  prettyDoc (TCWTypeclassUse _ tcn matches) =
    prettyDoc tcn <> " : " <> fromString (show matches)
  prettyDoc (TCWSubstitution sub) = prettyDoc sub

filterSubstitutions :: [TCWrite ann] -> [Substitution ResolvedDep ann]
filterSubstitutions =
  mapMaybe
    ( \case
        TCWSubstitution sub -> Just sub
        _ -> Nothing
    )

filterTypeclassUses :: [TCWrite ann] -> [(ResolvedDep Identifier, TypeclassName, [(Identifier, Integer)])]
filterTypeclassUses =
  mapMaybe
    ( \case
        TCWTypeclassUse ident s matches -> Just (ident, s, matches)
        _ -> Nothing
    )
