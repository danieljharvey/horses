import type {
  ExprHash,
  ModuleHash,
  ProjectHash,
  ExpressionData,
  BindingVersion,
  ExprUsage,
} from '../../types'

export type StoreItem = {
  expression: ExpressionData
}

export type ProjectState = {
  store: Record<ExprHash, StoreItem>
  moduleStore: Record<ModuleHash, unknown>
  projectHash: ProjectHash
  bindings: Record<string, ExprHash>
  typeBindings: Record<string, ExprHash>
  modules: Record<string, ModuleHash>
  versions: Record<string, BindingVersion[]>
  usages: Record<ExprHash, ExprUsage[]>
}
