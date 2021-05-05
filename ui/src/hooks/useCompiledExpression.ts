import * as React from 'react'
import { compileStoreExpression } from '../service/compile'
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
  chExprHash: ExprHash
  chRuntime: string
}

export const useCompiledExpression = (
  chExprHash: ExprHash,
  chRuntime: string
) => {
  const def: State = {
    compiled: {
      type: 'Empty',
    },
    chExprHash,
    chRuntime,
  }

  const [state, setState] = React.useState<State>(def)

  if (
    state.chExprHash !== chExprHash ||
    state.chRuntime !== chRuntime
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
    compileStoreExpression({
      chExprHash,
      chRuntime,
    })
      .then(response => {
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
