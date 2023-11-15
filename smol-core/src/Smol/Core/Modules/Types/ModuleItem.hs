{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Smol.Core.Modules.Types.ModuleItem
  ( ModuleItem (..),
    ModuleExpression (..),
    ModuleType (..),
    ModuleDataType (..),
    ModuleInstance (..),
  )
where

import Data.Foldable (foldl')
import Prettyprinter
import Smol.Core.Modules.Types.TestName
import Smol.Core.Printer
import Smol.Core.Typecheck.Typeclass.Types
import Smol.Core.Types.DataType
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
import Smol.Core.Types.ParseDep
import Smol.Core.Types.Type

-- import Smol.Core.Types.TypeName

-- a module is, broadly, one file
-- it defines some datatypes, infixes and definitions
-- and it probably exports one or more of those

-- item parsed from file, kept like this so we can order them and have
-- duplicates
-- we will remove duplicates when we work out dependencies between everything
data ModuleItem ann
  = ModuleExpression (ModuleExpression ann)
  | ModuleType (ModuleType ann)
  | ModuleDataType (ModuleDataType ann)
  | ModuleTest TestName (Expr ParseDep ann)
  | ModuleInstance (ModuleInstance ann)
  | ModuleClass (Typeclass ParseDep ann)
  deriving stock (Eq, Ord, Functor)

deriving stock instance
  ( Show ann,
    Show (DataType ParseDep ann)
  ) =>
  Show (ModuleItem ann)

-- a top level expression
data ModuleExpression ann = ModuleExpressionC
  { meAnn :: ann,
    meIdent :: Identifier,
    meArgs :: [Identifier],
    meExpr :: Expr ParseDep ann
  }
  deriving stock (Eq, Ord, Functor)

deriving stock instance
  ( Show ann,
    Show (DataType ParseDep ann)
  ) =>
  Show (ModuleExpression ann)

-- a top level type signature
data ModuleType ann = ModuleTypeC
  { mtAnn :: ann,
    mtIdent :: Identifier,
    mtConstraints :: [Constraint ParseDep ann],
    mtType :: Type ParseDep ann
  }
  deriving stock (Eq, Ord, Functor)

deriving stock instance
  ( Show ann,
    Show (DataType ParseDep ann)
  ) =>
  Show (ModuleType ann)

-- a top level data type declaration
data ModuleDataType ann = ModuleDataTypeC
  { mdtAnn :: ann,
    mdtDataType :: DataType ParseDep ann
  }
  deriving stock (Eq, Ord, Functor)

deriving stock instance
  ( Show ann,
    Show (DataType ParseDep ann)
  ) =>
  Show (ModuleDataType ann)

-- a top level data type declaration
data ModuleInstance ann = ModuleInstanceC
  { miAnn :: ann,
    miConstraints :: [Constraint ParseDep ann],
    miHead :: Constraint ParseDep ann,
    miExpr :: Expr ParseDep ann
  }
  deriving stock (Eq, Ord, Functor)

deriving stock instance
  ( Show ann,
    Show (DataType ParseDep ann)
  ) =>
  Show (ModuleInstance ann)

instance Printer (ModuleItem ann) where
  prettyDoc (ModuleExpression (ModuleExpressionC {meIdent, meArgs, meExpr})) =
    printExpression meIdent meArgs meExpr <> line <> line
  prettyDoc (ModuleType (ModuleTypeC {mtConstraints, mtIdent, mtType})) =
    printType mtConstraints mtIdent mtType <> line
  prettyDoc (ModuleDataType (ModuleDataTypeC {mdtDataType})) =
    prettyDoc mdtDataType <> line <> line
  prettyDoc (ModuleTest testName expr) =
    printTest testName expr <> line <> line
  prettyDoc (ModuleInstance (ModuleInstanceC {miConstraints, miHead, miExpr})) =
    printInstance miConstraints miHead miExpr <> line <> line
  prettyDoc (ModuleClass moduleClass) =
    prettyDoc moduleClass <> line <> line

_withDoubleLines :: [Doc a] -> Doc a
_withDoubleLines = vsep . fmap (line <>)

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Int -> Doc style -> Doc style
indentMulti i doc = flatAlt (indent i doc) doc

printMany :: (Printer a) => [a] -> Doc style
printMany = foldl' (\doc a -> doc <+> prettyDoc a) mempty

printInstance ::
  [Constraint ParseDep ann] ->
  Constraint ParseDep ann ->
  Expr ParseDep ann ->
  Doc style
printInstance constraints instanceHead expr =
  let prettyConstraints = case constraints of
        [] -> " "
        cons ->
          "("
            <> concatWith
              (\a b -> a <> ", " <> b)
              (prettyDoc <$> cons)
            <> ") => "
   in "instance"
        <> prettyConstraints
        <> prettyDoc instanceHead
        <+> "="
        <> line
        <> indentMulti 2 (prettyDoc expr)

printType :: [Constraint ParseDep ann] -> Identifier -> Type ParseDep ann -> Doc style
printType constraints name ty =
  let prettyConstraints = case constraints of
        [] -> " "
        cons ->
          "("
            <> concatWith
              (\a b -> a <> ", " <> b)
              (prettyDoc <$> cons)
            <> ") =>"
   in "def"
        <+> prettyDoc name
        <+> ":"
        <> prettyConstraints
        <> line
        <> indentMulti 2 (prettyDoc ty)

printExpression :: Identifier -> [Identifier] -> Expr ParseDep ann -> Doc style
printExpression name args expr =
  "def"
    <+> prettyDoc name
    <> printMany args
    <+> "="
    <> line
    <> indentMulti 2 (prettyDoc expr)

printTest :: TestName -> Expr ParseDep ann -> Doc style
printTest testName expr =
  "test" <+> dquotes (prettyDoc testName) <+> "=" <> line <> indentMulti 2 (prettyDoc expr)
