{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Typechecker.Environment (lookupConstructor) where

import Control.Monad.Except
import qualified Data.Map as M
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Types.Modules.ModuleName

-- given a constructor name, return the type it lives in
lookupConstructor ::
  (MonadError (TypeErrorF var Annotation) m) =>
  Environment ->
  Annotation ->
    Maybe ModuleName -> 
  TyCon ->
  m DataType
lookupConstructor env ann modName name = do
  case M.toList $ M.filter (containsConstructor name) (getDataTypes env) of
    [(_, a)] -> pure a -- we only want a single match
    (_ : _) -> throwError (ConflictingConstructors ann name)
    _ -> throwError (TypeConstructorNotInScope env ann modName name)

-- does this data type contain the given constructor?
containsConstructor :: TyCon -> DataType -> Bool
containsConstructor name (DataType _tyName _tyVars constructors) =
  M.member name constructors
