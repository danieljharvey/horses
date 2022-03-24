{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Interpreter.InterpretVar where

import Data.String
import Language.Mimsa.Printer
import Language.Mimsa.Types.Store.ExprHash

data InterpretVar var
  = ILocal var
  | IImport ExprHash
  deriving stock (Eq, Ord, Show)

instance (Printer var) => Printer (InterpretVar var) where
  prettyPrint (ILocal var) = prettyPrint var
  prettyPrint (IImport exprHash) = prettyPrint exprHash

instance (IsString var) => IsString (InterpretVar var) where
  fromString = ILocal . fromString
