module Smol.Core.Typecheck.Types.TCWrite (TCWrite(..)) where

import Smol.Core.Typecheck.Types.Substitution
import Smol.Core.Types.ResolvedDep

-- stuff emitted during typechecking
data TCWrite ann
  = TCWSubstitution (Substitution ResolvedDep ann)

