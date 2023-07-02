{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smol.Core.Modules.Types.DepType
  ( DepType (..),
  )
where

import Smol.Core
import Smol.Core.Types.Module.TopLevelExpression

data DepType dep ann
  = DTExpr (TopLevelExpression dep ann)
  | DTData (DataType dep ann)

deriving stock instance
  ( Eq ann,
    Eq (dep Identifier),
    Eq (dep Constructor),
    Eq (dep TypeName)
  ) =>
  Eq (DepType dep ann)

deriving stock instance
  ( Show ann,
    Show (dep Identifier),
    Show (dep Constructor),
    Show (dep TypeName)
  ) =>
  Show (DepType dep ann)
