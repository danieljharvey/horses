import {
  EvaluateModuleRequest,
  EvaluateModuleResponse,
  ListBindingsRequest,
  ListBindingsResponse,
  CreateProjectResponse,
  UserErrorResponse,
  BindModuleRequest,
  BindModuleResponse,
} from '../types/'
import {
  axiosPost,
  axiosGet,
} from '../utils/axios-taskeither'
import * as TE from 'fp-ts/TaskEither'

// project-based API calls

const baseUrl = process.env.REACT_APP_MIMSA_API_URL

export const evaluateModule = (
  evaluateRequest: EvaluateModuleRequest
): TE.TaskEither<
  UserErrorResponse,
  EvaluateModuleResponse
> =>
  axiosPost(
    `${baseUrl}/project/evaluate-module/`,
    evaluateRequest
  )

export const bindModule = (
  bindModuleRequest: BindModuleRequest
): TE.TaskEither<UserErrorResponse, BindModuleResponse> =>
  axiosPost(
    `${baseUrl}/project/bind-module/`,
    bindModuleRequest
  )

export const listBindings = (
  listBindingsRequest: ListBindingsRequest
): TE.TaskEither<string, ListBindingsResponse> =>
  axiosPost(
    `${baseUrl}/project/bindings/`,
    listBindingsRequest
  )

export const createProject = (): TE.TaskEither<
  string,
  CreateProjectResponse
> => axiosGet(`${baseUrl}/project/create`)
