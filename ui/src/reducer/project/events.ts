import {
  ModuleHash,
  ProjectHash,
  projectHash,
} from '../../types'
import { ProjectState } from './types'

export const initialProject = (
  projHash: string
): ProjectState => ({
  store: {},
  moduleStore: {},
  projectHash: projectHash(projHash),
  modules: {},
})

export const fetchModule = (moduleHash: ModuleHash) => ({
  type: 'FetchModule' as const,
  moduleHash,
})

export const saveToSessionStorage = (
  projectHash: ProjectHash
) => ({
  type: 'SaveToSessionStorage' as const,
  projectHash,
})

export const listBindings = (projectHash: ProjectHash) => ({
  type: 'ListBindings' as const,
  projectHash,
})

export type ProjectEvent =
  | ReturnType<typeof saveToSessionStorage>
  | ReturnType<typeof listBindings>
  | ReturnType<typeof fetchModule>
