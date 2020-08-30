module Language.Mimsa.Typechecker.Environment where

import Control.Monad.Except
import qualified Data.Map as M
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types

-- given a constructor name, return the type it lives in
lookupConstructor ::
  Environment ->
  Construct ->
  TcMonad DataType
lookupConstructor env name =
  case M.toList $ M.filter (containsConstructor name) (getDataTypes env) of
    [(_, a)] -> pure a -- we only want a single match
    (_ : _) -> throwError (ConflictingConstructors name)
    _ -> throwError (TypeConstructorNotInScope env name)

-- does this data type contain the given constructor?
containsConstructor :: Construct -> DataType -> Bool
containsConstructor name (DataType _tyName _tyVars constructors) =
  M.member name constructors
