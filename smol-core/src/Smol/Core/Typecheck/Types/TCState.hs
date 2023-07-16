{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Smol.Core.Typecheck.Types.TCState
  ( TCState (..),
    GlobalMap (..),
    filterIdent,
    globalMapIsNull,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Smol.Core.Types

newtype GlobalMap ann = GlobalMap {getGlobalMap :: Map Identifier (ResolvedType ann)}
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

globalMapIsNull :: GlobalMap ann -> Bool
globalMapIsNull (GlobalMap m) = M.null m

filterIdent :: Identifier -> GlobalMap ann -> GlobalMap ann
filterIdent ident (GlobalMap m) =
  GlobalMap $
    M.delete ident m

data TCState ann = TCState
  { tcsArgStack :: [ResolvedType ann],
    tcsUnknown :: Integer,
    tcsGlobals :: [GlobalMap ann]
  }
