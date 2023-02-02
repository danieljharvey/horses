{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Smol.Core.Types.Module.Module
  ( Module (..),
    DefPart (..),
    ModuleItem (..),
    Import (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics
import Prettyprinter
import Smol.Core.Printer
import Smol.Core.Types.Annotated
import Smol.Core.Types.DataType
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
import Smol.Core.Types.Module.DefIdentifier
import Smol.Core.Types.Module.ModuleHash
import Smol.Core.Types.Module.ModuleName
import Smol.Core.Types.ParseDep
import Smol.Core.Types.Type
import Smol.Core.Types.TypeName

-- a module is, broadly, one file
-- it defines some datatypes, infixes and definitions
-- and it probably exports one or more of those

data DefPart ann
  = -- | typeless argument `a`
    DefArg (Annotated Identifier ann)
  | -- | argument with type `(a: String) ->`
    DefTypedArg (Annotated Identifier ann) (ParsedType ann)
  | -- | type with no binding `String`
    DefType (ParsedType ann)
  deriving stock (Eq, Ord, Show, Functor)

-- item parsed from file, kept like this so we can order them and have
-- duplicates
-- we will remove duplicates when we work out dependencies between everything
-- TODO: add more annotations to everything so we can produce clearer errors
-- when things don't make sense (duplicate defs etc)
data ModuleItem ann
  = ModuleExpression Identifier [DefPart ann] (ParsedExpr ann)
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
data Module ann = Module
  { moExpressions :: Map DefIdentifier (ParsedExpr ann),
    moExpressionExports :: Set DefIdentifier,
    moExpressionImports :: Map DefIdentifier ModuleHash, -- what we imported, where it's from
    moDataTypes :: Map TypeName (DataType ParseDep ann),
    moDataTypeExports :: Set TypeName, -- which types to export
    moDataTypeImports :: Map TypeName ModuleHash, -- what we imported, where its from,
    moNamedImports :: Map ModuleName ModuleHash -- `import sdfsdf as Prelude`..
  }
  deriving stock (Eq, Ord, Functor, Generic)

deriving stock instance
  ( Show ann,
    Show (DataType ParseDep ann)
  ) =>
  Show (Module ann)

deriving anyclass instance
  ( ToJSON ann,
    ToJSON (DataType ParseDep ann)
  ) =>
  ToJSON (Module ann)

deriving anyclass instance
  (FromJSON ann, FromJSON (DataType ParseDep ann)) =>
  FromJSON (Module ann)

instance Printer (Module ann) where
  prettyDoc mod' =
    let printedDefs =
          uncurry (printDefinition mod')
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

printTypeDef :: Module ann -> TypeName -> DataType ParseDep ann -> Doc style
printTypeDef mod' tn dt =
  let prettyExp =
        if S.member tn (moDataTypeExports mod')
          then "export "
          else ""
   in prettyExp <> prettyDoc dt

-- given annotation and expr, pair annotation types with lambdas
printPaired :: ParsedType ann -> ParsedExpr ann -> Doc style
printPaired (TFunc _ _ fn arg) (ELambda _ ident body) =
  "(" <> prettyDoc ident
    <+> ":"
    <+> prettyDoc fn
      <> ")"
      <> line
      <> printPaired arg body
printPaired mt expr =
  ":"
    <+> prettyDoc mt
    <+> "="
      <> line
      <> indentMulti 2 (prettyDoc expr)

printDefinition :: Module ann -> DefIdentifier -> ParsedExpr ann -> Doc a
printDefinition mod' def expr =
  let prettyExp =
        if S.member def (moExpressionExports mod')
          then "export "
          else ""
   in prettyExp <> case def of
        DIName name -> case expr of
          (EAnn _ mt rest) ->
            "def"
              <+> prettyDoc name
                <> line
                <> indentMulti 2 (printPaired mt rest)
          other ->
            "def"
              <+> prettyDoc name
              <+> "="
                <> line
                <> indentMulti 2 (prettyDoc other)
        DIType _ -> error "printDefinition is printing type oh no"

{- DIInfix infixOp ->
          "infix" <+> prettyDoc infixOp <+> "=" <+> prettyDoc expr
        DITest testName ->
          "test" <+> "\"" <> prettyDoc testName <> "\"" <+> "=" <+> prettyDoc expr
-}

instance Semigroup (Module ann) where
  (Module a b c d e f g) <> (Module a' b' c' d' e' f' g') =
    Module (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g')

instance Monoid (Module ann) where
  mempty =
    Module
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
