import {
  EvaluateRequest,
  EvaluateResponse,
  EvaluateModuleRequest,
  EvaluateModuleResponse,
  ListBindingsRequest,
  ListBindingsResponse,
  CreateProjectResponse,
  BindExpressionRequest,
  BindExpressionResponse,
  BindTypeRequest,
  BindTypeResponse,
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

export const evaluate = (
  evaluateRequest: EvaluateRequest
): TE.TaskEither<UserErrorResponse, EvaluateResponse> =>
  axiosPost(`${baseUrl}/project/evaluate/`, evaluateRequest)

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

export const bindExpression = (
  bindExpressionRequest: BindExpressionRequest
): TE.TaskEither<
  UserErrorResponse,
  BindExpressionResponse
> =>
  axiosPost(
    `${baseUrl}/project/bind/`,
    bindExpressionRequest
  )

export const bindModule = (
  bindModuleRequest: BindModuleRequest
): TE.TaskEither<UserErrorResponse, BindModuleResponse> =>
  axiosPost(
    `${baseUrl}/project/bind-module/`,
    bindModuleRequest
  )

export const bindType = (
  bindTypeRequest: BindTypeRequest
): TE.TaskEither<UserErrorResponse, BindTypeResponse> =>
  axiosPost(`${baseUrl}/project/type/`, bindTypeRequest)

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
