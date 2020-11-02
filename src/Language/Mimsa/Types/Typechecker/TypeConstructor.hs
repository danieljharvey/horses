{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker.TypeConstructor where

import qualified Data.Text as T
import Language.Mimsa.Printer (Printer (prettyPrint))
import Language.Mimsa.Types.Identifiers (TyCon)
import Language.Mimsa.Types.Typechecker.MonoType (MonoType)

data TypeConstructor
  = TypeConstructor
      { tcConsName :: TyCon,
        tcTypeVars :: [MonoType],
        tcConsTypes :: [MonoType]
      }
  deriving (Show)

instance Printer TypeConstructor where
  prettyPrint (TypeConstructor consName _tyTypeVars consTypes) =
    prettyPrint consName <> " " <> T.intercalate " " (prettyPrint <$> consTypes)
