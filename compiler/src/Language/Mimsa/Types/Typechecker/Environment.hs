{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker.Environment where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Printer (Printer (prettyPrint))
import Language.Mimsa.Types.AST (DataType)
import Language.Mimsa.Types.AST.InfixOp
import Language.Mimsa.Types.Identifiers
  ( TyCon,
    TyVar,
    TypeIdentifier,
  )
import Language.Mimsa.Types.Typechecker.MonoType
import Language.Mimsa.Types.Typechecker.Scheme (Scheme)

-- everything we need in typechecking environment
data Environment = Environment
  { getSchemes :: Map TypeIdentifier Scheme,
    getDataTypes :: Map TyCon DataType,
    getInfix :: Map InfixOp MonoType,
    getTypeVarsInScope :: Map TyVar Int
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup Environment where
  (Environment a b c d) <> (Environment a' b' c' d') =
    Environment (a <> a') (b <> b') (c <> c') (d <> d')

instance Monoid Environment where
  mempty = Environment mempty mempty mempty mempty

instance Printer Environment where
  prettyPrint (Environment typeSchemes _dataTypes _infix _tyVars) =
    "[\n"
      <> T.intercalate ", \n" (printRow <$> M.toList typeSchemes)
      <> "\n]"
    where
      printRow (var, scheme) =
        prettyPrint var <> ": " <> prettyPrint scheme
