{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.TypeConstructor where

import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.MonoType

data TypeConstructor
  = TypeConstructor
      { tcConsName :: Construct,
        tcTypeVars :: [MonoType],
        tcConsTypes :: [MonoType]
      }
  deriving (Show)

instance Printer TypeConstructor where
  prettyPrint (TypeConstructor consName _tyTypeVars consTypes) =
    prettyPrint consName <> " " <> T.intercalate " " (prettyPrint <$> consTypes)
