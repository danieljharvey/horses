import axios from 'axios'
import { Either, left, right } from 'fp-ts/lib/Either'
import { CompileExpressionRequest } from '../types/'

// project-based API calls

const baseUrl = process.env.REACT_APP_MIMSA_API_URL

type Binary = any

export const compileExpression = (
  compileExpressionRequest: CompileExpressionRequest
): Promise<Either<string, Binary>> =>
  axios
    .post(
      `${baseUrl}/compile/expression/`,
      compileExpressionRequest,
      {
        responseType: 'blob',
      }
    )
    .then((a) => a.data)
    .then(right)
    .catch((e) => Promise.resolve(left(e.response.data)))
