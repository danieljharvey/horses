{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smol.Core.Modules.Types.DepType
  ( DepType (..),
  )
where

import Smol.Core

data DepType dep ann
  = DTExpr (Expr dep ann)
  | DTData (DataType dep ann)

deriving stock instance
  ( Eq ann,
    Eq (dep Identifier),
    Eq (dep Constructor),
    Eq (dep TypeName)
  ) =>
  Eq (DepType dep ann)
