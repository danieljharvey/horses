{-# LANGUAGE DeriveGeneric #-}

module Language.Mimsa.Types.Variable where

import GHC.Generics
import Language.Mimsa.Types.Name

data BiIds
  = NoId
  | OneId Variable
  | TwoIds Variable Variable
  | ThreeIds Variable Variable Variable
  deriving (Eq, Ord, Show, Generic)

data Variable
  = NamedVar Name
  | NumberedVar Int
  | BuiltIn Name
  | BuiltInActual Name BiIds
  deriving (Eq, Ord, Show, Generic)
