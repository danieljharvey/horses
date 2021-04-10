import * as React from 'react'
import { compileExpression } from '../service/compile'
import * as E from 'fp-ts/Either'
import { ExprHash } from '../types'

type CompiledState =
  | { type: 'Empty' }
  | { type: 'Fetching' }
  | { type: 'Failed' }
  | { type: 'CreatingBlob' }
  | { type: 'HasBlob'; url: string }

type State = {
  compiled: CompiledState
  ceProjectHash: ExprHash
  ceExpression: string
}

export const useCompiledExpression = (
  ceProjectHash: ExprHash,
  ceExpression: string,
  ceRuntime: string
) => {
  const def: State = {
    compiled: {
      type: 'Empty',
    },
    ceExpression,
    ceProjectHash,
  }

  const [state, setState] = React.useState<State>(def)

  if (
    state.ceExpression !== ceExpression ||
    state.ceProjectHash !== ceProjectHash
  ) {
    setState(def)
  }

  const compile = () => {
    if (
      state.compiled.type === 'Fetching' ||
      state.compiled.type === 'CreatingBlob'
    ) {
      return
    }

    setState({ ...state, compiled: { type: 'Fetching' } })
    compileExpression({
      ceExpression,
      ceProjectHash,
      ceRuntime,
    })
      .then((response) => {
        console.log('compile response', response)
        if (E.isLeft(response)) {
          setState({
            ...state,
            compiled: { type: 'Failed' },
          })
          throw new Error('nah')
        } else {
          setState({
            ...state,
            compiled: { type: 'CreatingBlob' },
          })
          return response.right
        }
      })
      .then((blob: any) => {
        let url = window.URL.createObjectURL(blob)
        setState({
          ...state,
          compiled: { type: 'HasBlob', url },
        })
      })
  }

  return [state.compiled, compile] as const
}
