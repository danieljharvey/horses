{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker.TypeConstructor where

import qualified Data.Text as T
import Language.Mimsa.Core

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
