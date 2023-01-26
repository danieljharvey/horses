{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Smol.Core.Typecheck.Types
  ( TCState (..),
    TCEnv (..),
    TCError (..),
    VarType (..),
    GlobalMap (..),
    filterIdent,
    globalMapIsNull,
  )
where

import Control.Monad.Identity
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Smol.Core.Types
import Smol.Core.Types.Constructor
  ( Constructor,
  )
import Smol.Core.Types.DataType (DataType)
import Smol.Core.Types.Expr (Op)
import Smol.Core.Types.Identifier (Identifier)
import Smol.Core.Types.Pattern (Pattern)
import Smol.Core.Types.PatternMatchError (PatternMatchError)
import Smol.Core.Types.ResolvedDep
import Smol.Core.Types.Type (Type)
import Smol.Core.Types.TypeName (TypeName)

newtype GlobalMap ann = GlobalMap {getGlobalMap :: Map Identifier (ResolvedType ann)}
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

globalMapIsNull :: GlobalMap ann -> Bool
globalMapIsNull (GlobalMap m) = M.null m

filterIdent :: Identifier -> GlobalMap ann -> GlobalMap ann
filterIdent ident (GlobalMap m) =
  GlobalMap $
    M.delete ident m

data VarType = Variable | Global
  deriving stock (Eq, Ord, Show)

data TCError ann
  = TCUnknownError
  | TCCouldNotFindVar (ResolvedDep Identifier)
  | TCCouldNotFindGlobal Identifier
  | TCTypeMismatch (ResolvedType ann) (ResolvedType ann)
  | TCExpectedFunction (ResolvedType ann)
  | TCTupleSizeMismatch Int (ResolvedType ann)
  | TCExpectedTuple (ResolvedType ann)
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
    tceDataTypes :: Map TypeName (DataType ResolvedDep ann)
  }

data TCState ann = TCState
  { tcsArgStack :: [ResolvedType ann],
    tcsUnknown :: Integer,
    tcsGlobals :: [GlobalMap ann]
  }
