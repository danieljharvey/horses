module Language.Mimsa.Backend.Typescript.FromDataType (fromDataType) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.Mimsa.Backend.Typescript.FromType
import Language.Mimsa.Backend.Typescript.Monad
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Core
import Language.Mimsa.Types.Error

fromDataType ::
  TSReaderState ->
  TSCodegenState ->
  DataType ->
  Either (BackendError MonoType) TSDataType
fromDataType readerState startState dt = do
  (result, _, _) <- runTypescriptM readerState startState (toTSDataType dt)
  pure result

toTSDataType :: DataType -> TypescriptM TSDataType
toTSDataType (DataType name gens cons) = do
  let toTSCons (tyCon, con) = do
        tsTypes' <- traverse toTSType con
        pure $ TSConstructor tyCon (fst <$> tsTypes')
  tsTypes <- traverse toTSCons (M.toList cons)
  pure $
    TSDataType
      name
      (T.toTitle . prettyPrint <$> gens)
      tsTypes
