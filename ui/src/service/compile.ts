import type { CompileModuleRequest } from '../types/'
import { axiosPost } from '../utils/axios-taskeither'
import * as TE from 'fp-ts/TaskEither'

// project-based API calls

const baseUrl = process.env.REACT_APP_MIMSA_API_URL

type Binary = any

export const compileModule = (
  compileModuleRequest: CompileModuleRequest
): TE.TaskEither<string, Binary> =>
  axiosPost(
    `${baseUrl}/compile/hash/`,
    compileModuleRequest,
    {
      responseType: 'blob',
    }
  )
