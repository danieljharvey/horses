{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Smol.Modules.Helpers
  ( filterNameDefs,
    filterTypeDefs,
    mapKey
  )
where

import Data.Bifunctor (first)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Smol.Core
import Smol.Core.Helpers (filterMapKeys)
import Smol.Modules.Types.DefIdentifier

filterNameDefs :: Map (DefIdentifier dep) a -> Map Identifier a
filterNameDefs =
  filterMapKeys
    ( \case
        DIName name -> Just name
        _ -> Nothing
    )

filterTypeDefs :: Map (DefIdentifier dep) a -> Map TypeName a
filterTypeDefs =
  filterMapKeys
    ( \case
        DIType typeName -> Just typeName
        _ -> Nothing
    )

mapKey :: (Ord k1) => (k -> k1) -> Map k a -> Map k1 a
mapKey f = M.fromList . fmap (first f) . M.toList

