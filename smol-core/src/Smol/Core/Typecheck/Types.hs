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

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Smol.Core.Types.Constructor
  ( Constructor,
  )
import Smol.Core.Types.DataType (DataType)
import Smol.Core.Types.Expr (Op)
import Smol.Core.Types.Identifier (Identifier)
import Smol.Core.Types.ParseDep
import Smol.Core.Types.Pattern (Pattern)
import Smol.Core.Types.PatternMatchError (PatternMatchError)
import Smol.Core.Types.ResolvedDep
import Smol.Core.Types.Type (Type)
import Smol.Core.Types.TypeName (TypeName)

newtype GlobalMap ann = GlobalMap {getGlobalMap :: Map Identifier (Type ann)}
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
  | TCTypeMismatch (Type ann) (Type ann)
  | TCExpectedFunction (Type ann)
  | TCTupleSizeMismatch Int (Type ann)
  | TCExpectedTuple (Type ann)
  | TCRecordMissingItems (Set Identifier)
  | TCExpectedRecord (Type ann)
  | TCInfixMismatch Op (Type ann) (Type ann)
  | TCPatternMismatch (Pattern ResolvedDep ann) (Type ann)
  | TCUnknownConstructor (ResolvedDep Constructor) [Constructor]
  | TCConstructorArgumentMismatch (ResolvedDep Constructor) Int Int -- expected, actual
  | TCExpectedConstructorType (Type ann)
  | TCCompoundTypeInEquality (Type ann) -- for now we only do primitive equality
  | TCPatternMatchError (PatternMatchError (Type ann))
  deriving stock (Eq, Ord, Show, Foldable)

data TCEnv ann = TCEnv
  { tceVars :: Map (ResolvedDep Identifier) (Type ann),
    tceGlobals :: Map Identifier (Type ann),
    tceDataTypes :: Map TypeName (DataType ann)
  }

data TCState ann = TCState
  { tcsArgStack :: [Type ann],
    tcsUnknown :: Integer,
    tcsGlobals :: [GlobalMap ann]
  }
