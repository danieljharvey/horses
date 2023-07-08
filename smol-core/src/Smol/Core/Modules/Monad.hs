{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Modules.Monad
  ( errorIfExpressionAlreadyDefined,
    checkDataType,
  )
where

import Control.Monad (when)
import Control.Monad.Except
import Data.Coerce
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Smol.Core
import Smol.Core.Modules.Types.ModuleError
import Smol.Core.Modules.Types.Module

errorIfExpressionAlreadyDefined ::
  (MonadError ModuleError m) =>
  Module dep ann ->
  Identifier ->
  m ()
errorIfExpressionAlreadyDefined mod' def =
  when
    ( M.member def (moExpressions mod')
    )
    (throwError (DuplicateDefinition def))

checkDataType ::
  (MonadError ModuleError m) =>
  Module dep ann ->
  DataType dep ann ->
  m ()
checkDataType mod' (DataType typeName _ constructors) = do
  errorIfTypeAlreadyDefined mod' (coerce typeName)
  traverse_ (errorIfConstructorAlreadyDefined mod') (M.keys constructors)

errorIfTypeAlreadyDefined ::
  (MonadError ModuleError m) =>
  Module dep ann ->
  TypeName ->
  m ()
errorIfTypeAlreadyDefined mod' typeName =
  when
    ( M.member typeName (moDataTypes mod')
    )
    (throwError (DuplicateTypeName typeName))

errorIfConstructorAlreadyDefined ::
  (MonadError ModuleError m) =>
  Module dep ann ->
  Constructor ->
  m ()
errorIfConstructorAlreadyDefined mod' tyCon =
  let allCons = mconcat (M.keysSet . dtConstructors <$> M.elems (moDataTypes mod'))
   in when
        (S.member tyCon allCons)
        (throwError (DuplicateConstructor tyCon))
