module Language.Mimsa.Types.ResolverError where

import Language.Mimsa.Types.Bindings
import Language.Mimsa.Types.Name

data ResolverError = MissingBinding Name Bindings
  deriving (Eq, Ord, Show)
