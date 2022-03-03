import { ExprHash } from '../../types'
import { ProjectState } from './types'

export const initialProject = (
  projectHash: string
): ProjectState => ({
  store: {},
  projectHash,
  bindings: {},
  typeBindings: {},
  versions: {},
  usages: {},
})

export const fetchExpressions = (
  hashes: ExprHash[],
  projectHash: ExprHash
) => ({
  type: 'FetchExpressions' as const,
  hashes,
  projectHash,
})

export const saveToSessionStorage = (
  projectHash: ExprHash
) => ({
  type: 'SaveToSessionStorage' as const,
  projectHash,
})

export const listBindings = (projectHash: ExprHash) => ({
  type: 'ListBindings' as const,
  projectHash,
})

export type ProjectEvent =
  | ReturnType<typeof saveToSessionStorage>
  | ReturnType<typeof listBindings>
  | ReturnType<typeof fetchExpressions>
