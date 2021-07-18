import {
  EvaluateRequest,
  EvaluateResponse,
  ListBindingsRequest,
  ListBindingsResponse,
  CreateProjectResponse,
  BindExpressionRequest,
  BindExpressionResponse,
  GetExpressionResponse,
  GetExpressionRequest,
  AddUnitTestRequest,
  AddUnitTestResponse,
  BindTypeRequest,
  BindTypeResponse,
  GraphProjectResponse,
  ExprHash,
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
): TE.TaskEither<string, EvaluateResponse> =>
  axiosPost(`${baseUrl}/project/evaluate/`, evaluateRequest)

export const bindExpression = (
  bindExpressionRequest: BindExpressionRequest
): TE.TaskEither<string, BindExpressionResponse> =>
  axiosPost(
    `${baseUrl}/project/bind/`,
    bindExpressionRequest
  )

export const bindType = (
  bindTypeRequest: BindTypeRequest
): TE.TaskEither<string, BindTypeResponse> =>
  axiosPost(`${baseUrl}/project/type/`, bindTypeRequest)

export const addUnitTest = (
  addUnitTestRequest: AddUnitTestRequest
): TE.TaskEither<string, AddUnitTestResponse> =>
  axiosPost(
    `${baseUrl}/project/tests/add/`,
    addUnitTestRequest
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

export const getExpression = (
  getExpressionRequest: GetExpressionRequest
): TE.TaskEither<string, GetExpressionResponse> =>
  axiosPost(
    `${baseUrl}/project/expression/`,
    getExpressionRequest
  )

export const getProjectGraph = (
  projectHash: ExprHash
): TE.TaskEither<string, GraphProjectResponse> =>
  axiosGet(`${baseUrl}/project/graph/${projectHash}/`)
