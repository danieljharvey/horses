import type {
  GetModuleResponse,
  UserErrorResponse,
  ModuleHash,
} from '../types/'
import { axiosGet } from '../utils/axios-taskeither'
import * as TE from 'fp-ts/TaskEither'

// project-based API calls

const baseUrl = process.env.REACT_APP_MIMSA_API_URL

export const getModule = (
  moduleHash: ModuleHash
): TE.TaskEither<UserErrorResponse, GetModuleResponse> =>
  axiosGet(`${baseUrl}/module/${moduleHash}`)
