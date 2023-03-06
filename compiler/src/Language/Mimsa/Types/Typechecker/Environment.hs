{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker.Environment where

import Data.HashMap.Strict (HashMap)
import Language.Mimsa.Core
import Language.Mimsa.Types.Typechecker.Scheme (Scheme)
import Prettyprinter

-- everything we need in typechecking environment
data Environment = Environment
  { getSchemes :: HashMap TypeIdentifier Scheme,
    getDataTypes :: HashMap (Maybe ModuleName, TypeName) DataType,
    getInfix :: HashMap InfixOp Scheme,
    getTypeVarsInScope :: HashMap TyVar Int, -- used for scoping type variables
    getNamespacedSchemes :: HashMap ModuleHash (HashMap Name Scheme) -- Name should probably be DefIdentifier or something so we can do Infix in future
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup Environment where
  (Environment a b c d e) <> (Environment a' b' c' d' e') =
    Environment (a <> a') (b <> b') (c <> c') (d <> d') (e <> e')

instance Monoid Environment where
  mempty = Environment mempty mempty mempty mempty mempty

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Int -> Doc style -> Doc style
indentMulti i doc = flatAlt (indent i doc) doc

instance Printer Environment where
  prettyDoc (Environment typeSchemes dataTypes infixes tyVars externals) =
    "["
      <> indentMulti
        2
        ( line
            <> "typeSchemes:"
            <> indentMulti 2 (prettyDoc typeSchemes)
            <> comma
            <> line
            <> "dataTypes:"
            <> indentMulti 2 (prettyDoc dataTypes)
            <> comma
            <> line
            <> "infixes:"
            <> indentMulti 2 (prettyDoc infixes)
            <> comma
            <> line
            <> "tyVars:"
            <> indentMulti 2 (prettyDoc tyVars)
            <> comma
            <> line
            <> "externals:"
            <> indentMulti 2 (prettyDoc externals)
        )
      <> line
      <> "]"
