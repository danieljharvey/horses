import * as React from 'react'
import { compileStoreExpression } from '../service/compile'
import * as E from 'fp-ts/Either'
import { ExprHash } from '../types'

type CompiledState =
  | { type: 'Empty' }
  | { type: 'Fetching' }
  | { type: 'Failed' }
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

  const compile = async () => {
    if (state.compiled.type === 'Fetching') {
      return
    }

    setState({ ...state, compiled: { type: 'Fetching' } })
    const resp = await compileStoreExpression({
      chExprHash,
      chRuntime,
    })()

    if (E.isLeft(resp)) {
      setState({ ...state, compiled: { type: 'Failed' } })
    } else {
      let url = window.URL.createObjectURL(resp.right)
      setState({
        ...state,
        compiled: { type: 'HasBlob', url },
      })
    }
  }

  return [state.compiled, compile] as const
}
