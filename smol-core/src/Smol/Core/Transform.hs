module Smol.Core.Transform (transform) where

import Smol.Core.Transform.BetaReduce
import Smol.Core.Transform.ConstantFold
import Smol.Core.Transform.EtaReduce
import Smol.Core.Transform.FindUnused
import Smol.Core.Transform.FlattenLets
import Smol.Core.Transform.FloatDown
import Smol.Core.Transform.FloatUp
import Smol.Core.Transform.Helpers (repeatUntilEq)
import Smol.Core.Transform.Inliner
import Smol.Core.Types.Constructor
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
import Smol.Core.Types.TypeName

transform ::
  (Ord ann, Ord (dep Identifier), Eq (dep TypeName), Eq (dep Constructor)) =>
  Expr dep ann ->
  Expr dep ann
transform = repeatUntilEq transformPass

transformPass ::
  (Ord ann, Ord (dep Identifier), Eq (dep TypeName), Eq (dep Constructor)) =>
  Expr dep ann ->
  Expr dep ann
transformPass =
  removeUnused
    . etaReduce
    . floatDown
    . flattenLets
    . floatUp
    . betaReduce
    . inline
    . constantFold
