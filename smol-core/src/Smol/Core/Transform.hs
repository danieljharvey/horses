module Smol.Core.Transform (transform) where

import Smol.Core.Transform.BetaReduce
import Smol.Core.Transform.EtaReduce
import Smol.Core.Transform.FindUnused
import Smol.Core.Transform.FlattenLets
import Smol.Core.Transform.FloatDown
import Smol.Core.Transform.FloatUp
import Smol.Core.Transform.Inliner
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier

transform ::
  (Ord ann, Ord (dep Identifier)) =>
  Expr dep ann ->
  Expr dep ann
transform = removeUnused . etaReduce . betaReduce . floatDown . floatUp . flattenLets . inline
