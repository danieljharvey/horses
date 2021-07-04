import * as React from 'react'
import * as E from 'fp-ts/Either'
import { ExprHash } from '../types'
import { routerFetchExpression } from '../service/deploy'

type DeployedState =
  | { type: 'Empty' }
  | { type: 'Fetching' }
  | { type: 'Failed'; error: string }
  | { type: 'HasDeployment'; url: string }

type State = {
  deploy: DeployedState
  chExprHash: ExprHash
}

export const useDeployedExpression = (
  chExprHash: ExprHash
) => {
  const def: State = {
    deploy: {
      type: 'Empty',
    },
    chExprHash,
  }

  const [state, setState] = React.useState<State>(def)

  if (state.chExprHash !== chExprHash) {
    setState(def)
  }

  const compile = async () => {
    if (state.deploy.type === 'Fetching') {
      return
    }

    setState({ ...state, deploy: { type: 'Fetching' } })

    const resp = await routerFetchExpression(chExprHash)()

    if (E.isLeft(resp)) {
      setState({
        ...state,
        deploy: { type: 'Failed', error: resp.left },
      })
    } else {
      setState({
        ...state,
        deploy: {
          type: 'HasDeployment',
          url: resp.right,
        },
      })
    }
  }

  return [state.deploy, compile] as const
}
