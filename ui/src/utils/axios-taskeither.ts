import axios, { AxiosRequestConfig } from 'axios'
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

export const axiosPost = <Data, Response>(
  url: string,
  data: Data,
  config?: AxiosRequestConfig
): TE.TaskEither<string, Response> =>
  pipe(
    TE.tryCatch(
      () => axios.post(url, data, config),
      (e: any) => {
        if (e.response?.data) {
          return e.response.data
        }
        return 'Unknown error'
      }
    ),
    TE.map((a) => a.data)
  )

export const axiosGet = <Response>(
  url: string,
  config?: AxiosRequestConfig
): TE.TaskEither<string, Response> =>
  pipe(
    TE.tryCatch(
      () => axios.get(url, config),
      (e: any) => {
        if (e.response?.data) {
          return e.response.data
        }
        return 'Unknown error'
      }
    ),
    TE.map((a) => a.data)
  )
