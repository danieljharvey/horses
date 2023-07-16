{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Smol.Core.Typecheck.Types
  ( TCState (..),
    TCEnv (..),
    TCError (..),
    GlobalMap (..),
    filterIdent,
    globalMapIsNull,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Smol.Core.Types
import Smol.Core.Types.PatternMatchError (PatternMatchError)

newtype GlobalMap ann = GlobalMap {getGlobalMap :: Map Identifier (ResolvedType ann)}
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

globalMapIsNull :: GlobalMap ann -> Bool
globalMapIsNull (GlobalMap m) = M.null m

filterIdent :: Identifier -> GlobalMap ann -> GlobalMap ann
filterIdent ident (GlobalMap m) =
  GlobalMap $
    M.delete ident m

data TCError ann
  = TCUnknownError
  | TCCouldNotFindVar ann (ResolvedDep Identifier)
  | TCCouldNotFindGlobal Identifier (Set Identifier)
  | TCTypeMismatch (ResolvedType ann) (ResolvedType ann)
  | TCTupleSizeMismatch Int (ResolvedType ann)
  | TCExpectedTuple (ResolvedType ann)
  | TCExpectedFunction (ResolvedType ann)
  | TCRecordMissingItems (Set Identifier)
  | TCExpectedRecord (ResolvedType ann)
  | TCInfixMismatch Op (ResolvedType ann) (ResolvedType ann)
  | TCPatternMismatch (Pattern ResolvedDep ann) (ResolvedType ann)
  | TCUnknownConstructor (ResolvedDep Constructor) [Constructor]
  | TCConstructorArgumentMismatch (ResolvedDep Constructor) Int Int -- expected, actual
  | TCExpectedConstructorType (ResolvedType ann)
  | TCCompoundTypeInEquality (ResolvedType ann) -- for now we only do primitive equality
  | TCPatternMatchError (PatternMatchError (ResolvedType ann))
  deriving stock (Eq, Ord, Show, Foldable)

data TCEnv ann = TCEnv
  { tceVars :: Map (ResolvedDep Identifier) (ResolvedType ann),
    tceGlobals :: Map Identifier (ResolvedType ann),
    tceDataTypes :: Map (ResolvedDep TypeName) (DataType ResolvedDep ann)
  }

data TCState ann = TCState
  { tcsArgStack :: [ResolvedType ann],
    tcsUnknown :: Integer,
    tcsGlobals :: [GlobalMap ann]
  }
