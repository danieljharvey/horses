module Language.Mimsa.Types.Scheme where

import Language.Mimsa.Types.MonoType

data Scheme = Scheme [TypeVar] MonoType
  deriving (Eq, Ord, Show)
