{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Environment where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Printer (Printer (prettyPrint))
import Language.Mimsa.Types.AST (DataType)
import Language.Mimsa.Types.Identifiers (TyCon, Variable)
import Language.Mimsa.Types.Scheme (Scheme)

-- everything we need in typechecking environment
data Environment = Environment
  { getSchemes :: Map Variable Scheme,
    getDataTypes :: Map TyCon DataType
  }
  deriving (Eq, Ord, Show)

instance Semigroup Environment where
  (Environment a b) <> (Environment a' b') =
    Environment (a <> a') (b <> b')

instance Monoid Environment where
  mempty = Environment mempty mempty

instance Printer Environment where
  prettyPrint (Environment typeSchemes _dataTypes) =
    "[\n"
      <> T.intercalate ", \n" (printRow <$> M.toList typeSchemes)
      <> "\n]"
    where
      printRow (var, scheme) =
        prettyPrint var <> ": " <> prettyPrint scheme
