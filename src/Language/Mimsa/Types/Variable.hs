{-# LANGUAGE DeriveGeneric #-}

module Language.Mimsa.Types.Variable where

import qualified Data.Text as T
import GHC.Generics
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Printer

data BiIds
  = NoId
  | OneId Int
  | TwoIds Int Int
  | ThreeIds Int Int Int
  deriving (Eq, Ord, Show, Generic)

data Variable
  = NamedVar Name
  | NumberedVar Int
  | BuiltIn Name
  | BuiltInActual Int BiIds
  deriving (Eq, Ord, Show, Generic)

instance Printer Variable where
  prettyPrint (NamedVar n) = prettyPrint n
  prettyPrint (NumberedVar i) = T.pack (show i)
  prettyPrint (BuiltIn n) = prettyPrint n
  prettyPrint (BuiltInActual n _) = prettyPrint n
