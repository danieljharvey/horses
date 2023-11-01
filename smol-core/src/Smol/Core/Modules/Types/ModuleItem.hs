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

import Prettyprinter
import Smol.Core.Modules.Types.TestName
import Smol.Core.Printer
import Smol.Core.Typecheck.Typeclass.Types
import Smol.Core.Types.DataType
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
import Smol.Core.Types.ParseDep
import Smol.Core.Types.Type
--import Smol.Core.Types.TypeName

-- a module is, broadly, one file
-- it defines some datatypes, infixes and definitions
-- and it probably exports one or more of those

-- item parsed from file, kept like this so we can order them and have
-- duplicates
-- we will remove duplicates when we work out dependencies between everything
-- TODO: add more annotations to everything so we can produce clearer errors
-- when things don't make sense (duplicate defs etc)
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
  prettyDoc (ModuleExpression (ModuleExpressionC {meIdent,meExpr})) =
    printExpression meIdent meExpr <> line <> line
  prettyDoc (ModuleType (ModuleTypeC {mtIdent,mtType})) =
    printType mtIdent mtType <> line
  prettyDoc (ModuleDataType (ModuleDataTypeC {})) = mempty
  prettyDoc (ModuleTest _ _) = mempty
  prettyDoc (ModuleInstance _) = mempty
  prettyDoc (ModuleClass _) = mempty

_withDoubleLines :: [Doc a] -> Doc a
_withDoubleLines = vsep . fmap (line <>)

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Int -> Doc style -> Doc style
indentMulti i doc = flatAlt (indent i doc) doc

printType :: Identifier -> Type ParseDep ann -> Doc style
printType name ty =
          "def"
            <+> prettyDoc name
            <+> ":"
            <> line
            <> indentMulti 2 (prettyDoc ty)
            <> "\n"

printExpression :: Identifier -> Expr ParseDep ann -> Doc a
printExpression name expr =
        "def"
          <+> prettyDoc name
          <+> "="
          <> line
          <> indentMulti 2 (prettyDoc expr)
