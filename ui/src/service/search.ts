import type {
  TypeSearchRequest,
  TypeSearchResponse,
} from '../types/'
import { axiosPost } from '../utils/axios-taskeither'
import * as TE from 'fp-ts/TaskEither'

// search-based API calls

const baseUrl = process.env.REACT_APP_MIMSA_API_URL

export const typeSearch = (
  typeSearchRequest: TypeSearchRequest
): TE.TaskEither<string, TypeSearchResponse> =>
  axiosPost(`${baseUrl}/search/type/`, typeSearchRequest)
