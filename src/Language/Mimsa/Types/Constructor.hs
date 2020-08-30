{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Constructor where

import qualified Data.Text as T
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Printer

data Constructor
  = Constructor
      { cConsName :: Construct,
        vTypeVars :: [MonoType],
        vConsTypes :: [MonoType]
      }

instance Printer Constructor where
  prettyPrint (Constructor consName _tyTypeVars consTypes) =
    prettyPrint consName <> " " <> T.intercalate " " (prettyPrint <$> consTypes)
