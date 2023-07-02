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

module Smol.Core.Types.Module.Module
  ( Module (..),
    ModuleItem (..),
    Import (..),
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Prettyprinter
import Smol.Core.Printer
import Smol.Core.Types.Constructor
import Smol.Core.Types.DataType
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
import Smol.Core.Types.Module.DefIdentifier
import Smol.Core.Types.Module.ModuleHash
import Smol.Core.Types.Module.ModuleName
import Smol.Core.Types.Module.TopLevelExpression
import Smol.Core.Types.ParseDep
import Smol.Core.Types.Type
import Smol.Core.Types.TypeName

-- a module is, broadly, one file
-- it defines some datatypes, infixes and definitions
-- and it probably exports one or more of those

-- item parsed from file, kept like this so we can order them and have
-- duplicates
-- we will remove duplicates when we work out dependencies between everything
-- TODO: add more annotations to everything so we can produce clearer errors
-- when things don't make sense (duplicate defs etc)
data ModuleItem ann
  = ModuleExpression Identifier [Identifier] (ParsedExpr ann)
  | ModuleExpressionType Identifier (Type ParseDep ann)
  | ModuleDataType (DataType ParseDep ann)
  | ModuleExport (ModuleItem ann)
  | ModuleImport Import
  --  | ModuleInfix InfixOp (ParsedExpr ann)
  --  | ModuleTest TestName (ParsedExpr ann)
  deriving stock (Eq, Ord, Functor)

deriving stock instance
  ( Show ann,
    Show (DataType ParseDep ann)
  ) =>
  Show (ModuleItem ann)

-- going to want way more granularity here in future but _shrug_
data Import
  = ImportAllFromHash ModuleHash
  | ImportNamedFromHash ModuleHash ModuleName
  deriving stock (Eq, Ord, Show)

-- this is the checked module, it contains no duplicates and we don't care
-- about ordering
data Module dep ann = Module
  { moExpressions :: Map DefIdentifier (TopLevelExpression dep ann),
    moExpressionExports :: Set DefIdentifier,
    moExpressionImports :: Map DefIdentifier ModuleHash, -- what we imported, where it's from
    moDataTypes :: Map TypeName (DataType dep ann),
    moDataTypeExports :: Set TypeName, -- which types to export
    moDataTypeImports :: Map TypeName ModuleHash, -- what we imported, where its from,
    moNamedImports :: Map ModuleName ModuleHash -- `import sdfsdf as Prelude`..
  }
  deriving stock (Functor, Generic)

deriving stock instance
  ( Eq ann,
    Eq (dep TypeName),
    Eq (dep Identifier),
    Eq (dep Constructor)
  ) =>
  Eq (Module dep ann)

deriving stock instance
  ( Ord ann,
    Ord (dep TypeName),
    Ord (dep Constructor),
    Ord (dep Identifier)
  ) =>
  Ord (Module dep ann)

deriving stock instance
  ( Show ann,
    Show (dep TypeName),
    Show (dep Constructor),
    Show (dep Identifier)
  ) =>
  Show (Module dep ann)

deriving anyclass instance
  ( ToJSONKey (dep Identifier),
    ToJSON ann,
    ToJSON (dep TypeName),
    ToJSON (dep Constructor),
    ToJSON (dep Identifier)
  ) =>
  ToJSON (Module dep ann)

deriving anyclass instance
  ( Ord (dep Identifier),
    FromJSONKey (dep Identifier),
    FromJSON ann,
    FromJSON (dep TypeName),
    FromJSON (dep Constructor),
    FromJSON (dep Identifier)
  ) =>
  FromJSON (Module dep ann)

instance Printer (Module ParseDep ann) where
  prettyDoc mod' =
    let printedDefs =
          uncurry printDefinition
            <$> M.toList (moExpressions mod')
        printedTypes =
          uncurry (printTypeDef mod')
            <$> M.toList (moDataTypes mod')
        printedImports =
          printImport
            <$> uniq
              ( M.elems (moDataTypeImports mod')
                  <> M.elems (moExpressionImports mod')
              )
        printedNamedImports =
          printNamedImport <$> M.toList (moNamedImports mod')
     in withDoubleLines
          ( printedImports
              <> printedTypes
              <> printedDefs
              <> printedNamedImports
          )

withDoubleLines :: [Doc a] -> Doc a
withDoubleLines = vsep . fmap (line <>)

uniq :: (Ord a) => [a] -> [a]
uniq = S.toList . S.fromList

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Int -> Doc style -> Doc style
indentMulti i doc = flatAlt (indent i doc) doc

printNamedImport :: (ModuleName, ModuleHash) -> Doc style
printNamedImport (modName, modHash) =
  "import" <+> prettyDoc modName <+> "from" <+> prettyDoc modHash

printImport :: ModuleHash -> Doc a
printImport modHash =
  "import * from" <+> prettyDoc modHash

printTypeDef :: Module ParseDep ann -> TypeName -> DataType ParseDep ann -> Doc style
printTypeDef mod' tn dt =
  let prettyExp =
        if S.member tn (moDataTypeExports mod')
          then "export "
          else ""
   in prettyExp <> prettyDoc dt

printDefinition :: DefIdentifier -> TopLevelExpression ParseDep ann -> Doc a
printDefinition def (TopLevelExpression {tleType, tleExpr}) =
  case def of
    DIName name ->
      let prettyExpr =
            "def"
              <+> prettyDoc name
              <+> "="
                <> line
                <> indentMulti 2 (prettyDoc tleExpr)
          prettyType = case tleType of
            Just ty ->
              "def"
                <+> prettyDoc name
                <+> ":"
                  <> line
                  <> indentMulti 2 (prettyDoc ty)
                  <> "\n"
            Nothing -> ""
       in prettyType <> prettyExpr
    DIType _ -> error "printDefinition is printing type oh no"

{- DIInfix infixOp ->
          "infix" <+> prettyDoc infixOp <+> "=" <+> prettyDoc expr
        DITest testName ->
          "test" <+> "\"" <> prettyDoc testName <> "\"" <+> "=" <+> prettyDoc expr
-}

instance Semigroup (Module dep ann) where
  (Module a b c d e f g) <> (Module a' b' c' d' e' f' g') =
    Module (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g')

instance Monoid (Module dep ann) where
  mempty =
    Module
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
