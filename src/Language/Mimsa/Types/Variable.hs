{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Variable where

import qualified Data.Text as T
import GHC.Generics
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Printer

data BiIds
  = NoId
  | OneId Variable
  | TwoIds Variable Variable
  | ThreeIds Variable Variable Variable
  deriving (Eq, Ord, Show, Generic)

instance Printer BiIds where
  prettyPrint NoId = "-"
  prettyPrint (OneId v1) = prettyPrint v1
  prettyPrint (TwoIds v1 v2) = T.intercalate ", " (prettyPrint <$> [v1, v2])
  prettyPrint (ThreeIds v1 v2 v3) = T.intercalate ", " (prettyPrint <$> [v1, v2, v3])

data Variable
  = NamedVar Name
  | NumberedVar Int
  | BuiltIn Name
  | BuiltInActual Name BiIds
  deriving (Eq, Ord, Show, Generic)

instance Printer Variable where
  prettyPrint (NamedVar n) = prettyPrint n
  prettyPrint (NumberedVar i) = T.pack (show i)
  prettyPrint (BuiltIn n) = prettyPrint n
  prettyPrint (BuiltInActual n _) = prettyPrint n
