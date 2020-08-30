{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.TypeConstructor where

import qualified Data.Text as T
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Printer

data TypeConstructor
  = TypeConstructor
      { tcConsName :: Construct,
        tcTypeVars :: [MonoType],
        tcConsTypes :: [MonoType]
      }

instance Printer TypeConstructor where
  prettyPrint (TypeConstructor consName _tyTypeVars consTypes) =
    prettyPrint consName <> " " <> T.intercalate " " (prettyPrint <$> consTypes)
