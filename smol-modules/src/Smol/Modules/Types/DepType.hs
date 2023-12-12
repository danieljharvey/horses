{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smol.Modules.Types.DepType
  ( DepType (..),
  )
where

import Smol.Core
import Smol.Modules.Types.TopLevelExpression
import Smol.Typecheck.Typeclass.Types

data DepType dep ann
  = DTExpr (TopLevelExpression dep ann)
  | DTInstance (Instance dep ann)
  | DTData (DataType dep ann)
  | DTTest (Expr dep ann)

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
