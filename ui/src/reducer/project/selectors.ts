import { Lens } from 'monocle-ts'
import * as R from 'fp-ts/Record'
import * as O from 'fp-ts/Option'
import { pipe, identity } from 'fp-ts/function'
import {
  BindingVersion,
  ExprHash,
  exprHash,
  ProjectHash,
  projectHash,
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

const versionsL = Lens.fromPath<State>()([
  'project',
  'versions',
])

const usagesL = Lens.fromPath<State>()([
  'project',
  'usages',
])

export const getUsagesOfExprHash =
  (state: State) =>
  (exprHash: ExprHash): ExprUsage[] =>
    pipe(
      usagesL.get(state),
      R.lookup(exprHash),
      O.fold(() => [], identity)
    )

export const getVersionsOfBinding =
  (state: State) =>
  (bindingName: string): BindingVersion[] =>
    pipe(
      versionsL.get(state),
      R.lookup(bindingName),
      O.fold(() => [], identity)
    )

// how many versions of this binding are in active use in the project?
export const countActiveVersionsOfBinding =
  (state: State) =>
  (bindingName: string): number => {
    const versions =
      getVersionsOfBinding(state)(bindingName)
    return versions
      .map((version) =>
        getUsagesOfExprHash(state)(
          exprHash(version.bvExprHash)
        )
      )
      .filter((usage) => usage.length > 0).length
  }

export const getProjectHash = (state: State): ProjectHash =>
  projectHash(state.project.projectHash)

export const lookupNameForExprHash =
  (state: State) =>
  (exprHash: ExprHash): O.Option<string> =>
    pipe(
      O.fromNullable(
        Object.keys(state.project.bindings).find(
          (k) => state.project.bindings[k] === exprHash
        )
      ),
      O.alt(() =>
        O.fromNullable(
          Object.keys(state.project.typeBindings).find(
            (k) =>
              state.project.typeBindings[k] === exprHash
          )
        )
      )
    )
