module Language.Mimsa.Types.Scheme where

import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Name

data Scheme = Scheme [Name] MonoType
  deriving (Eq, Ord, Show)
