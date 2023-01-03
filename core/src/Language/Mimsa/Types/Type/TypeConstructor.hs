{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Type.TypeConstructor where

import qualified Data.Text as T
import Language.Mimsa.Printer (Printer (prettyPrint))
import Language.Mimsa.Types.Identifiers.TypeName
import Language.Mimsa.Types.Module.ModuleName
import Language.Mimsa.Types.Type.MonoType (MonoType)

data TypeConstructor = TypeConstructor
  { tcModName :: Maybe ModuleName,
    tcConsName :: TypeName,
    tcTypeVars :: [MonoType],
    tcConsTypes :: [MonoType]
  }
  deriving stock (Show)

instance Printer TypeConstructor where
  prettyPrint (TypeConstructor modName consName tyTypeVars consTypes) =
    prefix <> prettyPrint consName <> " [" <> vars <> "] " <> cons
    where
      prefix = case modName of
        Just m -> prettyPrint m <> "."
        _ -> mempty
      vars = T.intercalate ", " (prettyPrint <$> tyTypeVars)
      cons = T.intercalate " " (prettyPrint <$> consTypes)
