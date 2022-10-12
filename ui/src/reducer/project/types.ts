import type {
  ExprHash,
  ModuleHash,
  ProjectHash,
  ExpressionData,
} from '../../types'

export type StoreItem = {
  expression: ExpressionData
}

export type ProjectState = {
  store: Record<ExprHash, StoreItem> // probably irrelevant now too?
  moduleStore: Record<ModuleHash, unknown>
  projectHash: ProjectHash
  modules: Record<string, ModuleHash>
}
