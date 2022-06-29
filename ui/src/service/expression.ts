import type {
  GetExpressionsResponse,
  UserErrorResponse,
  ExprHash,
} from '../types/'
import { axiosGet } from '../utils/axios-taskeither'
import * as TE from 'fp-ts/TaskEither'

// project-based API calls

const baseUrl = process.env.REACT_APP_MIMSA_API_URL

export const getExpressions = (
  exprHashes: ExprHash[]
): TE.TaskEither<
  UserErrorResponse,
  GetExpressionsResponse
> =>
  axiosGet(`${baseUrl}/expressions/${exprHashes.join(',')}`)
