import { Lens } from 'monocle-ts'
import * as R from 'fp-ts/Record'
import * as O from 'fp-ts/Option'
import { pipe, identity } from 'fp-ts/function'
import {
  BindingVersion,
  ExprHash,
  ExprUsage,
} from '../../types'
import { State } from '../types'

const bindingsL = Lens.fromPath<State>()([
  'project',
  'bindings',
])

export const getProjectBindings = (
  state: State
): string[] => Object.keys(bindingsL.get(state))

const typeBindingsL = Lens.fromPath<State>()([
  'project',
  'typeBindings',
])

export const getProjectTypeBindings = (
  state: State
): string[] => Object.keys(typeBindingsL.get(state))

const versionsL = Lens.fromPath<State>()([
  'project',
  'versions',
])

export const getProjectVersions = (
  state: State
): Record<string, BindingVersion[]> => versionsL.get(state)

const usagesL = Lens.fromPath<State>()([
  'project',
  'usages',
])

export const getUsagesOfExprHash = (
  exprHash: ExprHash,
  state: State
): ExprUsage[] =>
  pipe(
    usagesL.get(state),
    R.lookup(exprHash),
    O.fold(() => [], identity)
  )

export const getVersionsOfBinding = (
  bindingName: string,
  state: State
): BindingVersion[] =>
  pipe(
    versionsL.get(state),
    R.lookup(bindingName),
    O.fold(() => [], identity)
  )

// how many versions of this binding are in active use in the project?
export const countActiveVersionsOfBinding = (
  bindingName: string,
  state: State
): number => {
  const versions = getVersionsOfBinding(bindingName, state)
  return versions
    .map((version) =>
      getUsagesOfExprHash(version.bvExprHash, state)
    )
    .filter((usage) => usage.length > 0).length
}

export const getProjectHash = (state: State): ExprHash =>
  state.project.projectHash

export const lookupNameForExprHash = (
  exprHash: ExprHash,
  state: State
): O.Option<string> =>
  pipe(
    O.fromNullable(
      Object.keys(state.project.bindings).find(
        (k) => state.project.bindings[k] === exprHash
      )
    ),
    O.alt(() =>
      O.fromNullable(
        Object.keys(state.project.typeBindings).find(
          (k) => state.project.typeBindings[k] === exprHash
        )
      )
    )
  )
