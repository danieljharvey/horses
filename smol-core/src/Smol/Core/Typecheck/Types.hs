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
import qualified Smol.Core.Types.Constructor as Smol
  ( Constructor,
  )
import qualified Smol.Core.Types.DataType as Smol (DataType)
import qualified Smol.Core.Types.Expr as Smol (Op)
import qualified Smol.Core.Types.Identifier as Smol (Identifier)
import qualified Smol.Core.Types.Pattern as Smol (Pattern)
import Smol.Core.Types.PatternMatchError (PatternMatchError)
import qualified Smol.Core.Types.Type as Smol (Type)
import qualified Smol.Core.Types.TypeName as Smol (TypeName)

newtype GlobalMap ann = GlobalMap {getGlobalMap :: Map Smol.Identifier (Smol.Type ann)}
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

globalMapIsNull :: GlobalMap ann -> Bool
globalMapIsNull (GlobalMap m) = M.null m

filterIdent :: Smol.Identifier -> GlobalMap ann -> GlobalMap ann
filterIdent ident (GlobalMap m) =
  GlobalMap $
    M.delete ident m

data VarType = Variable | Global
  deriving stock (Eq, Ord, Show)

data TCError ann
  = TCUnknownError
  | TCCouldNotFindVar VarType Smol.Identifier
  | TCTypeMismatch (Smol.Type ann) (Smol.Type ann)
  | TCExpectedFunction (Smol.Type ann)
  | TCTupleSizeMismatch Int (Smol.Type ann)
  | TCExpectedTuple (Smol.Type ann)
  | TCRecordMissingItems (Set Smol.Identifier)
  | TCExpectedRecord (Smol.Type ann)
  | TCInfixMismatch Smol.Op (Smol.Type ann) (Smol.Type ann)
  | TCPatternMismatch (Smol.Pattern ann) (Smol.Type ann)
  | TCUnknownConstructor Smol.Constructor [Smol.Constructor]
  | TCConstructorArgumentMismatch Smol.Constructor Int Int -- expected, actual
  | TCExpectedConstructorType (Smol.Type ann)
  | TCCompoundTypeInEquality (Smol.Type ann) -- for now we only do primitive equality
  | TCPatternMatchError (PatternMatchError (Smol.Type ann))
  deriving stock (Eq, Ord, Show, Foldable)

data TCEnv ann = TCEnv
  { tceVars :: Map Smol.Identifier (Smol.Type ann),
    tceGlobals :: Map Smol.Identifier (Smol.Type ann),
    tceDataTypes :: Map Smol.TypeName (Smol.DataType ann)
  }

data TCState ann = TCState
  { tcsArgStack :: [Smol.Type ann],
    tcsUnknown :: Integer,
    tcsGlobals :: [GlobalMap ann]
  }
