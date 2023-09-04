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
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import Prettyprinter
import Smol.Core.Modules.Types.TestName
import Smol.Core.Modules.Types.TopLevelExpression
import Smol.Core.Printer
import Smol.Core.Typecheck.Typeclass.Types
import Smol.Core.Types.Constructor
import Smol.Core.Types.DataType
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
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
  = ModuleExpression Identifier [Identifier] (Expr ParseDep ann)
  | ModuleExpressionType Identifier [Constraint ParseDep ann] (Type ParseDep ann)
  | ModuleDataType (DataType ParseDep ann)
  | ModuleTest TestName Identifier
  | ModuleInstance [Constraint ParseDep ann] (Constraint ParseDep ann) (Expr ParseDep ann)
  | ModuleClass (Typeclass ParseDep ann)
  deriving stock (Eq, Ord, Functor)

deriving stock instance
  ( Show ann,
    Show (DataType ParseDep ann)
  ) =>
  Show (ModuleItem ann)

-- this is the checked module, it contains no duplicates and we don't care
-- about ordering
-- should we care about ordering? it would allow us to pretty print?
data Module dep ann = Module
  { moExpressions :: Map Identifier (TopLevelExpression dep ann),
    moDataTypes :: Map TypeName (DataType dep ann)
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
          uncurry printTypeDef
            <$> M.toList (moDataTypes mod')
     in withDoubleLines
          ( printedTypes
              <> printedDefs
          )

withDoubleLines :: [Doc a] -> Doc a
withDoubleLines = vsep . fmap (line <>)

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Int -> Doc style -> Doc style
indentMulti i doc = flatAlt (indent i doc) doc

printTypeDef :: TypeName -> DataType ParseDep ann -> Doc style
printTypeDef _tn =
  prettyDoc

printDefinition :: Identifier -> TopLevelExpression ParseDep ann -> Doc a
printDefinition name (TopLevelExpression {tleType, tleExpr}) =
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

instance Semigroup (Module dep ann) where
  (Module a b) <> (Module a' b') =
    Module (a <> a') (b <> b')

instance Monoid (Module dep ann) where
  mempty =
    Module
      mempty
      mempty
