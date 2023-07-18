{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Typecheck.Types.TCWrite (TCWrite (..), filterSubstitutions, filterTypeclassUses) where

import Data.Maybe (mapMaybe)
import Data.String
import Smol.Core.Printer
import Smol.Core.Typecheck.Types.Substitution
import Smol.Core.Types.Identifier
import Smol.Core.Types.ResolvedDep

-- stuff emitted during typechecking
data TCWrite ann
  = TCWSubstitution (Substitution ResolvedDep ann)
  | TCWTypeclassUse String [(Identifier, Integer)]
  deriving stock (Eq, Ord, Show)

instance (Show ann) => Printer (TCWrite ann) where
  prettyDoc (TCWTypeclassUse tcName matches) = fromString tcName <> " : " <> fromString (show matches)
  prettyDoc (TCWSubstitution sub) = prettyDoc sub

filterSubstitutions :: [TCWrite ann] -> [Substitution ResolvedDep ann]
filterSubstitutions =
  mapMaybe
    ( \case
        TCWSubstitution sub -> Just sub
        _ -> Nothing
    )

filterTypeclassUses :: [TCWrite ann] -> [(String, [(Identifier, Integer)])]
filterTypeclassUses =
  mapMaybe
    ( \case
        TCWTypeclassUse s matches -> Just (s, matches)
        _ -> Nothing
    )
