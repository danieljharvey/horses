import { ExprHash } from '../../types'
import { ProjectState, ProjectEvent } from './types'

export const initialProject = (
  projectHash: string
): ProjectState => ({
  store: {},
  projectHash,
  bindings: {},
  typeBindings: {},
})

export const createProject = (): ProjectEvent => ({
  type: 'CreateProject',
})

export const fetchExpressions = (
  hashes: ExprHash[],
  projectHash: ExprHash
): ProjectEvent => ({
  type: 'FetchExpressions',
  hashes,
  projectHash,
})

export const saveToSessionStorage = (
  projectHash: ExprHash
): ProjectEvent => ({
  type: 'SaveToSessionStorage',
  projectHash,
})

export const listBindings = (
  projectHash: ExprHash
): ProjectEvent => ({
  type: 'ListBindings',
  projectHash,
})
