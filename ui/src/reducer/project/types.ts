import type {
  ExprHash,
  ExpressionData,
  BindingVersion,
  ExprUsage,
} from '../../types'

export type StoreItem = {
  expression: ExpressionData
}

export type ProjectState = {
  store: Record<ExprHash, StoreItem>
  projectHash: ExprHash
  bindings: Record<string, ExprHash>
  typeBindings: Record<string, ExprHash>
  versions: Record<string, BindingVersion[]>
  usages: Record<ExprHash, ExprUsage[]>
}
