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
  AddUnitTestRequest,
  AddUnitTestResponse,
  BindTypeRequest,
  BindTypeResponse,
  ExprHash,
  ProjectHash,
  UserErrorResponse,
  ListTestsResponse,
} from '../types/'
import {
  axiosPost,
  axiosGet,
} from '../utils/axios-taskeither'
import * as TE from 'fp-ts/TaskEither'
import { ListTestsByExprHashResponse } from '../generated'

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

export const bindType = (
  bindTypeRequest: BindTypeRequest
): TE.TaskEither<UserErrorResponse, BindTypeResponse> =>
  axiosPost(`${baseUrl}/project/type/`, bindTypeRequest)

export const addUnitTest = (
  addUnitTestRequest: AddUnitTestRequest
): TE.TaskEither<UserErrorResponse, AddUnitTestResponse> =>
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

export const getProjectTests = (
  projectHash: ProjectHash
): TE.TaskEither<string, ListTestsResponse> =>
  axiosGet(`${baseUrl}/project/${projectHash}/tests/list`)

export const getTestsForExpression = (
  projectHash: ProjectHash,
  exprHash: ExprHash
): TE.TaskEither<string, ListTestsByExprHashResponse> =>
  axiosGet(
    `${baseUrl}/project/${projectHash}/tests/list/${exprHash}/`
  )
