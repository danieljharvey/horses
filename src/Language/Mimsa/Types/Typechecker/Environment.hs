{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker.Environment where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Printer (Printer (prettyPrint))
import Language.Mimsa.Types.AST (DataType)
import Language.Mimsa.Types.AST.Annotation
import Language.Mimsa.Types.AST.InfixOp
import Language.Mimsa.Types.Identifiers
  ( TyCon,
    TypeIdentifier,
  )
import Language.Mimsa.Types.Typechecker.MonoType
import Language.Mimsa.Types.Typechecker.Scheme (Scheme)

-- everything we need in typechecking environment
data Environment = Environment
  { getSchemes :: Map TypeIdentifier Scheme,
    getDataTypes :: Map TyCon (DataType Annotation),
    getInfix :: Map InfixOp MonoType
  }
  deriving (Eq, Ord, Show)

instance Semigroup Environment where
  (Environment a b c) <> (Environment a' b' c') =
    Environment (a <> a') (b <> b') (c <> c')

instance Monoid Environment where
  mempty = Environment mempty mempty mempty

instance Printer Environment where
  prettyPrint (Environment typeSchemes _dataTypes _infix) =
    "[\n"
      <> T.intercalate ", \n" (printRow <$> M.toList typeSchemes)
      <> "\n]"
    where
      printRow (var, scheme) =
        prettyPrint var <> ": " <> prettyPrint scheme
