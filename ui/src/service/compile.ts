import type { CompileHashRequest } from '../types/'
import { axiosPost } from '../utils/axios-taskeither'
import * as TE from 'fp-ts/TaskEither'

// project-based API calls

const baseUrl = process.env.REACT_APP_MIMSA_API_URL

type Binary = any

export const compileStoreExpression = (
  compileHashRequest: CompileHashRequest
): TE.TaskEither<string, Binary> =>
  axiosPost(
    `${baseUrl}/compile/hash/`,
    compileHashRequest,
    {
      responseType: 'blob',
    }
  )
