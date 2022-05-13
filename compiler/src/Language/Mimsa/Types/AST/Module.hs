{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.Module (Module (..), DefPart (..), ModuleItem (..)) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.DataType
import Language.Mimsa.Types.AST.Expr
import Language.Mimsa.Types.AST.Identifier
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Identifiers.TypeName
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

-- this is the checked module, it contains no duplicates and we don't care
-- about ordering
data Module ann = Module
  { moExpressions :: Map Name (Expr Name ann),
    moDataTypes :: Map TypeName DataType,
    moExpressionExports :: Set Name
  }
  deriving stock (Eq, Ord, Show, Functor)

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
  (Module exprs dts exports) <> (Module exprs' dts' exports') =
    Module (exprs <> exprs') (dts <> dts') (exports <> exports')

instance Monoid (Module ann) where
  mempty = Module mempty mempty mempty
