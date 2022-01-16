import { lookupNameForExprHash } from './selectors'
import * as O from 'fp-ts/Option'
import { initialState } from '../root'
import { State } from '../types'

const initial = initialState('blah')
const state: State = {
  ...initial,
  project: {
    ...initial.project,
    bindings: { test: '123' },
    typeBindings: { Hello: '234' },
  },
}

describe('lookupNameForExprHash', () => {
  it('Finds 123 in binding', () => {
    expect(lookupNameForExprHash('123', state)).toEqual(
      O.some('test')
    )
  })
  it('Finds 234 in type bindings', () => {
    expect(lookupNameForExprHash('234', state)).toEqual(
      O.some('Hello')
    )
  })
})
