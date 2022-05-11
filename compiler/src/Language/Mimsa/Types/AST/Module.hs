{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.Module (Module (..)) where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.DataType
import Language.Mimsa.Types.AST.Expr
import Language.Mimsa.Types.Identifiers.Name
import Language.Mimsa.Types.Identifiers.TypeName
import Language.Mimsa.Types.Typechecker.MonoType

-- one file, basically

data Module ann = Module
  { moExpressions :: Map Name (Expr Name ann),
    moTypeSignatures :: Map Name (Type ann),
    moDataTypes :: Map TypeName DataType
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
  (Module exprs ts dts) <> (Module exprs' ts' dts') =
    Module (exprs <> exprs') (ts <> ts') (dts <> dts')

instance Monoid (Module ann) where
  mempty = Module mempty mempty mempty
