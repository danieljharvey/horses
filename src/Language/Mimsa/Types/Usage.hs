module Language.Mimsa.Types.Usage where

import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.Name

data Usage
  = Transient Name ExprHash
  | Direct Name ExprHash
  deriving (Eq, Ord, Show)

----------

data UsageError
  = CouldNotResolveDeps [Name]
  | CouldNotFindBinding Name
  | CouldNotFindStoreExpression ExprHash
  deriving (Eq, Ord, Show)
