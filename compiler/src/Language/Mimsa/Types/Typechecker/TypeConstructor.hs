{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker.TypeConstructor where

import qualified Data.Text as T
import Language.Mimsa.Printer (Printer (prettyPrint))
import Language.Mimsa.Types.Identifiers (TyCon)
import Language.Mimsa.Types.Typechecker.MonoType (MonoType)
import Language.Mimsa.Types.Modules.ModuleName

data TypeConstructor = TypeConstructor
  { tcModName :: Maybe ModuleName,
    tcConsName :: TyCon,
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
