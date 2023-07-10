{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Smol.Core.Modules.Helpers
  (


    filterNameDefs,
    filterTypeDefs,
  )
where

import Data.Map.Strict (Map)
import Smol.Core
import Smol.Core.Helpers (filterMapKeys)
import Smol.Core.Modules.Types.DefIdentifier

filterNameDefs :: Map DefIdentifier a -> Map Identifier a
filterNameDefs =
  filterMapKeys
    ( \case
        DIName name -> Just name
        _ -> Nothing
    )

filterTypeDefs :: Map DefIdentifier a -> Map TypeName a
filterTypeDefs =
  filterMapKeys
    ( \case
        DIType typeName -> Just typeName
        _ -> Nothing
    )
