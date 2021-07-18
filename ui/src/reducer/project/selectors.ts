import { Lens } from 'monocle-ts'
import { State } from '../types'

const bindingsL = Lens.fromPath<State>()([
  'project',
  'bindings',
])

const typeBindingsL = Lens.fromPath<State>()([
  'project',
  'typeBindings',
])

export const getProjectBindings = (
  state: State
): string[] => Object.keys(bindingsL.get(state))

export const getProjectTypeBindings = (
  state: State
): string[] => Object.keys(typeBindingsL.get(state))
