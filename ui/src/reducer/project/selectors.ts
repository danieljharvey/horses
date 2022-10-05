import { Lens } from 'monocle-ts'
import * as R from 'fp-ts/Record'
import * as O from 'fp-ts/Option'
import { pipe, identity } from 'fp-ts/function'
import {
  BindingVersion,
  ExprHash,
  ProjectHash,
  projectHash,
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

export const getVersionsOfBinding =
  (state: State) =>
  (bindingName: string): BindingVersion[] =>
    pipe(
      versionsL.get(state),
      R.lookup(bindingName),
      O.fold(() => [], identity)
    )

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
