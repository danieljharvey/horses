module Language.Mimsa.Typechecker.Environment where

import Control.Monad.Except
import qualified Data.Map as M
import Language.Mimsa.Typechecker.TcMonad (TcMonad)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Environment
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers

-- given a constructor name, return the type it lives in
lookupConstructor ::
  Environment ->
  Annotation ->
  TyCon ->
  TcMonad DataType
lookupConstructor env ann name =
  case M.toList $ M.filter (containsConstructor name) (getDataTypes env) of
    [(_, a)] -> pure a -- we only want a single match
    (_ : _) -> throwError (ConflictingConstructors ann name)
    _ -> throwError (TypeConstructorNotInScope env ann name)

-- does this data type contain the given constructor?
containsConstructor :: TyCon -> DataType -> Bool
containsConstructor name (DataType _tyName _tyVars constructors) =
  M.member name constructors
