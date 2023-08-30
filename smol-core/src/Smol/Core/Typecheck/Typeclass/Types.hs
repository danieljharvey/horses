{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smol.Core.Typecheck.Typeclass.Types
  ( module Smol.Core.Typecheck.Typeclass.Types.Typeclass,
    module Smol.Core.Typecheck.Typeclass.Types.Constraint,
    module Smol.Core.Typecheck.Typeclass.Types.Instance,
    module Smol.Core.Typecheck.Typeclass.Types.TypeclassName,
  )
where

import Smol.Core.Typecheck.Typeclass.Types.Constraint
import Smol.Core.Typecheck.Typeclass.Types.Instance
import Smol.Core.Typecheck.Typeclass.Types.Typeclass
import Smol.Core.Typecheck.Typeclass.Types.TypeclassName
