import { ExprHash } from '../types'
import * as TE from 'fp-ts/TaskEither'
import { axiosPost } from '../utils/axios-taskeither'
import { pipe } from 'fp-ts/function'

// project-based API calls

const routerBaseUrl = process.env.REACT_APP_ROUTER_API_URL

export const routerFetchExpression = (
  feExprHash: ExprHash
): TE.TaskEither<string, string> =>
  pipe(
    axiosPost<{ feExprHash: string }, string, string>(
      `${routerBaseUrl}/fetch/expr`,
      {
        feExprHash,
      }
    ),
    TE.map((url) => `${routerBaseUrl}/${url}/`)
  )
