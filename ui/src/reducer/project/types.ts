import type { ModuleHash, ProjectHash } from '../../types'

export type ProjectState = {
  moduleStore: Record<ModuleHash, unknown>
  projectHash: ProjectHash
  modules: Record<string, ModuleHash>
}
