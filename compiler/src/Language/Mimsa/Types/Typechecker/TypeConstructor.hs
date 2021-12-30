{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker.TypeConstructor where

import qualified Data.Text as T
import Language.Mimsa.Printer (Printer (prettyPrint))
import Language.Mimsa.Types.Identifiers (TypeName)
import Language.Mimsa.Types.Typechecker.MonoType (MonoType)

-- | for putting together a monotype
data TypeConstructor = TypeConstructor
  { tcConsName :: TypeName,
    tcTypeVars :: [MonoType],
    tcConsTypes :: [MonoType]
  }
  deriving stock (Show)

instance Printer TypeConstructor where
  prettyPrint (TypeConstructor consName tyTypeVars consTypes) =
    prettyPrint consName <> " [" <> vars <> "] " <> cons
    where
      vars = T.intercalate ", " (prettyPrint <$> tyTypeVars)
      cons = T.intercalate " " (prettyPrint <$> consTypes)
