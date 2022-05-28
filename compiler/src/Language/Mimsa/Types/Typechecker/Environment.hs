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
  ( Name,
    TyCon,
    TyVar,
    TypeIdentifier,
  )
import Language.Mimsa.Types.Modules.ModuleHash
import Language.Mimsa.Types.Typechecker.Scheme (Scheme)
import Language.Mimsa.Types.Modules.ModuleName

-- everything we need in typechecking environment
data Environment = Environment
  { getSchemes :: Map TypeIdentifier Scheme,
    getDataTypes :: Map ( Maybe ModuleName, TyCon) DataType,
    getInfix :: Map InfixOp Scheme,
    getTypeVarsInScope :: Map TyVar Int, -- used for scoping type variables
    getNamespacedSchemes :: Map ModuleHash (Map Name Scheme) -- Name should probably be DefIdentifier or something so we can do Infix in future
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup Environment where
  (Environment a b c d e) <> (Environment a' b' c' d' e') =
    Environment (a <> a') (b <> b') (c <> c') (d <> d') (e <> e')

instance Monoid Environment where
  mempty = Environment mempty mempty mempty mempty mempty

-- TODO: this should include everything in order to be helpful
instance Printer Environment where
  prettyPrint (Environment typeSchemes _dataTypes _infix _tyVars _) =
    "[\n"
      <> T.intercalate ", \n" (printRow <$> M.toList typeSchemes)
      <> "\n]"
    where
      printRow (var, scheme) =
        prettyPrint var <> ": " <> prettyPrint scheme
