import {
  EvaluateRequest,
  EvaluateResponse,
  ListBindingsRequest,
  ListBindingsResponse,
  CreateProjectResponse,
  BindExpressionRequest,
  BindExpressionResponse,
  AddUnitTestRequest,
  AddUnitTestResponse,
  BindTypeRequest,
  BindTypeResponse,
  GraphProjectResponse,
  ExprHash,
  UserErrorResponse,
  ListTestsResponse,
  UpgradeRequest,
  UpgradeResponse,
} from '../types/'
import {
  axiosPost,
  axiosGet,
} from '../utils/axios-taskeither'
import * as TE from 'fp-ts/TaskEither'
import {
  ListTestsByExprHashResponse,
  OptimiseRequest,
  OptimiseResponse,
} from '../generated'

// project-based API calls

const baseUrl = process.env.REACT_APP_MIMSA_API_URL

export const evaluate = (
  evaluateRequest: EvaluateRequest
): TE.TaskEither<UserErrorResponse, EvaluateResponse> =>
  axiosPost(`${baseUrl}/project/evaluate/`, evaluateRequest)

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

export const getProjectGraph = (
  projectHash: ExprHash
): TE.TaskEither<string, GraphProjectResponse> =>
  axiosGet(`${baseUrl}/project/graph/${projectHash}/`)

export const getProjectTests = (
  projectHash: ExprHash
): TE.TaskEither<string, ListTestsResponse> =>
  axiosGet(`${baseUrl}/project/${projectHash}/tests/list`)

export const getTestsForExpression = (
  projectHash: ExprHash,
  exprHash: ExprHash
): TE.TaskEither<string, ListTestsByExprHashResponse> =>
  axiosGet(
    `${baseUrl}/project/${projectHash}/tests/list/${exprHash}/`
  )

export const upgradeExpression = (
  upgradeRequest: UpgradeRequest
): TE.TaskEither<UserErrorResponse, UpgradeResponse> =>
  axiosPost(`${baseUrl}/project/upgrade`, upgradeRequest)

export const optimiseExpression = (
  optimiseRequest: OptimiseRequest
): TE.TaskEither<UserErrorResponse, OptimiseResponse> =>
  axiosPost(`${baseUrl}/project/optimise`, optimiseRequest)
