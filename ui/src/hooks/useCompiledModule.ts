import * as React from 'react'
import { compileModule } from '../service/compile'
import * as E from 'fp-ts/Either'
import { Backend, ModuleHash } from '../types'

type CompiledState =
  | { type: 'Empty' }
  | { type: 'Fetching' }
  | { type: 'Failed' }
  | { type: 'HasBlob'; url: string }

type State = {
  compiled: CompiledState
  chModuleHash: ModuleHash
  chBackend: Backend
}

export const useCompiledModule = (
  chModuleHash: ModuleHash,
  chBackend: Backend
) => {
  const def: State = {
    compiled: {
      type: 'Empty',
    },
    chModuleHash,
    chBackend,
  }

  const [state, setState] = React.useState<State>(def)

  if (
    state.chModuleHash !== chModuleHash ||
    state.chBackend !== chBackend
  ) {
    setState(def)
  }

  const compile = async () => {
    if (state.compiled.type === 'Fetching') {
      return
    }

    setState({ ...state, compiled: { type: 'Fetching' } })
    const resp = await compileModule({
      chModuleHash,
      chBackend,
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
