module Language.Mimsa.Types.Project.Versioned (VersionedBindings, VersionedTypeBindings, VersionedModules) where

import Language.Mimsa.Types.Identifiers (Name, TyCon)
import Language.Mimsa.Types.Modules.ModuleHash
import Language.Mimsa.Types.Modules.ModuleName
import Language.Mimsa.Types.Project.VersionedMap (VersionedMap)
import Language.Mimsa.Types.Store (ExprHash)

type VersionedBindings = VersionedMap Name ExprHash

type VersionedTypeBindings = VersionedMap TyCon ExprHash

type VersionedModules = VersionedMap ModuleName ModuleHash
