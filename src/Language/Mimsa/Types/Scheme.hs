module Language.Mimsa.Types.Scheme where

import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Variable

data Scheme = Scheme [Variable] MonoType
  deriving (Eq, Ord, Show)
