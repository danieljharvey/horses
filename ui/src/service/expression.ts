import {
  GetExpressionResponse,
  UserErrorResponse,
  ExprHash,
} from '../types/'
import { axiosGet } from '../utils/axios-taskeither'
import * as TE from 'fp-ts/TaskEither'

// project-based API calls

const baseUrl = process.env.REACT_APP_MIMSA_API_URL
export const getExpression = (
  exprHash: ExprHash
): TE.TaskEither<
  UserErrorResponse,
  GetExpressionResponse
> => axiosGet(`${baseUrl}/expression/${exprHash}`)
