module Language.Mimsa.Types.Project.Versioned where

import Language.Mimsa.Types.Identifiers (Name, TyCon)
import Language.Mimsa.Types.Project.VersionedMap (VersionedMap)
import Language.Mimsa.Types.Store (ExprHash)

type VersionedBindings = VersionedMap Name ExprHash

type VersionedTypeBindings = VersionedMap TyCon ExprHash
