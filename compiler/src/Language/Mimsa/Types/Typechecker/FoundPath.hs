{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker.FoundPath (FoundPath (..), appendNameToFoundPath) where

import qualified Data.List.NonEmpty as NE
import Data.String
import qualified Data.Text as T
import Language.Mimsa.Core

-- | path to found item, ie ["state","map"]
newtype FoundPath = FoundPath (NE.NonEmpty Name)
  deriving stock (Eq, Ord, Show)

instance IsString FoundPath where
  fromString as =
    let textAs = T.pack as
     in case NE.nonEmpty (T.splitOn "." (T.pack as)) of
          Just path -> FoundPath (Name <$> path)
          Nothing -> FoundPath (NE.singleton (Name textAs))

instance Printer FoundPath where
  prettyPrint (FoundPath as) = T.intercalate "." (prettyPrint <$> NE.toList as)

appendNameToFoundPath :: Name -> FoundPath -> FoundPath
appendNameToFoundPath a (FoundPath as) =
  FoundPath (as <> NE.singleton a)
