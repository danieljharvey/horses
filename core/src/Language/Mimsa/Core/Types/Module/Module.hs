{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Core.Types.Module.Module
  ( Module (..),
    DefPart (..),
    ModuleItem (..),
    Import (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics
import Language.Mimsa.Core.Printer
import Language.Mimsa.Core.Types.AST.DataType
import Language.Mimsa.Core.Types.AST.Expr
import Language.Mimsa.Core.Types.AST.Identifier
import Language.Mimsa.Core.Types.AST.InfixOp
import Language.Mimsa.Core.Types.Identifiers
import Language.Mimsa.Core.Types.Module.DefIdentifier
import Language.Mimsa.Core.Types.Module.ModuleHash
import Language.Mimsa.Core.Types.Module.ModuleName
import Language.Mimsa.Core.Types.Type.MonoType
import Prettyprinter

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
  deriving stock (Eq, Ord, Show, Functor)

-- item parsed from file, kept like this so we can order them and have
-- duplicates
-- we will remove duplicates when we work out dependencies between everything
-- TODO: add more annotations to everything so we can produce clearer errors
-- when things don't make sense (duplicate defs etc)
data ModuleItem ann
  = ModuleExpression Name [DefPart ann] (Expr Name ann)
  | ModuleDataType DataType
  | ModuleExport (ModuleItem ann)
  | ModuleImport Import
  | ModuleInfix InfixOp (Expr Name ann)
  | ModuleTest TestName (Expr Name ann)
  deriving stock (Functor)

-- going to want way more granularity here in future but _shrug_
data Import
  = ImportAllFromHash ModuleHash
  | ImportNamedFromHash ModuleHash ModuleName

-- this is the checked module, it contains no duplicates and we don't care
-- about ordering
data Module ann = Module
  { moExpressions :: Map DefIdentifier (Expr Name ann),
    moExpressionExports :: Set DefIdentifier,
    moExpressionImports :: Map DefIdentifier ModuleHash, -- what we imported, where it's from
    moDataTypes :: Map TypeName DataType,
    moDataTypeExports :: Set TypeName, -- which types to export
    moDataTypeImports :: Map TypeName ModuleHash, -- what we imported, where its from,
    moNamedImports :: Map ModuleName ModuleHash -- `import sdfsdf as Prelude`..
  }
  deriving stock (Eq, Ord, Show, Functor, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)

instance (Show ann) => Printer (Module ann) where
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

printNamedImport :: (ModuleName, ModuleHash) -> Doc a
printNamedImport (modName, modHash) =
  "import" <+> prettyDoc modName <+> "from" <+> prettyDoc modHash

printImport :: ModuleHash -> Doc a
printImport modHash =
  "import * from" <+> prettyDoc modHash

printTypeDef :: Module ann -> TypeName -> DataType -> Doc a
printTypeDef mod' tn dt =
  let prettyExp =
        if S.member tn (moDataTypeExports mod')
          then "export "
          else ""
   in prettyExp <> prettyDoc dt

-- given annotation and expr, pair annotation types with lambdas
printPaired :: Type ann -> Expr Name ann -> Doc a
printPaired (MTFunction _ fn arg) (MyLambda _ ident body) =
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

printDefinition :: Module ann -> DefIdentifier -> Expr Name ann -> Doc a
printDefinition mod' def expr =
  let prettyExp =
        if S.member def (moExpressionExports mod')
          then "export "
          else ""
   in prettyExp <> case def of
        DIName name -> case expr of
          (MyAnnotation _ mt rest) ->
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
        DIInfix infixOp ->
          "infix" <+> prettyDoc infixOp <+> "=" <+> prettyDoc expr
        DIType _ -> error "printDefinition is printing type oh no"
        DITest testName ->
          "test" <+> "\"" <> prettyDoc testName <> "\"" <+> "=" <+> prettyDoc expr

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
