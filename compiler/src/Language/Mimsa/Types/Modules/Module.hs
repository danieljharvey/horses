{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Modules.Module
  ( Module (..),
    DefPart (..),
    ModuleItem (..),
    Import (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Text as T
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.DataType
import Language.Mimsa.Types.AST.Expr
import Language.Mimsa.Types.AST.Identifier
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Identifiers.TypeName
import Language.Mimsa.Types.Modules.ModuleHash
import Language.Mimsa.Types.Typechecker.MonoType

-- a module is, broadly, one file
-- it defines some datatypes, infixes and definitions
-- and it probably exports one or more of those

data DefPart ann
  = -- | typeless argument `a`
    DefArg (Identifier Name ann)
  | -- | argument with type `(a: String) ->`
    DefTypedArg (Identifier Name ann) (Type ann)
  | -- | type with no binding `String`
    DefType (Type ann)
  deriving stock (Eq, Ord, Show)

-- item parsed from file, kept like this so we can order them and have
-- duplicates
-- we will remove duplicates when we work out dependencies between everything
data ModuleItem ann
  = ModuleExpression Name [DefPart ann] (Expr Name ann)
  | ModuleDataType DataType
  | ModuleExport (ModuleItem ann)
  | ModuleImport Import

-- going to want way more granularity here in future but _shrug_
newtype Import = ImportAllFromHash ModuleHash

-- this is the checked module, it contains no duplicates and we don't care
-- about ordering
data Module ann = Module
  { moExpressions :: Map Name (Expr Name ann),
    moDataTypes :: Map TypeName DataType,
    moExpressionExports :: Set Name, -- might replace Name with a sum of things we can export instead
    moExpressionImports :: Map Name ModuleHash, -- what we imported, where it's from
    moDataTypeExports :: Set TypeName, -- which types to export
    moDataTypeImports :: Map TypeName ModuleHash -- what we imported, where its from
  }
  deriving stock (Eq, Ord, Show, Functor, Generic)
  deriving anyclass (JSON.ToJSON)

instance Printer (Module ann) where
  prettyPrint mod' =
    let printedDefs =
          T.intercalate
            "\n\n"
            ( ( \(name, expr) ->
                  "def " <> prettyPrint name <> " = "
                    <> prettyPrint expr
              )
                <$> M.toList (moExpressions mod')
            )
     in printedDefs

instance Semigroup (Module ann) where
  (Module a b c d e f) <> (Module a' b' c' d' e' f') =
    Module (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f')

instance Monoid (Module ann) where
  mempty = Module mempty mempty mempty mempty mempty mempty
