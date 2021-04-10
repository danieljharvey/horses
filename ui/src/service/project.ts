import axios from 'axios'
import { Either, left, right } from 'fp-ts/lib/Either'
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
} from '../types/'

// project-based API calls

const baseUrl = process.env.REACT_APP_MIMSA_API_URL

export const evaluate = (
    evaluateRequest: EvaluateRequest
): Promise<Either<string, EvaluateResponse>> =>
    axios
        .post(`${baseUrl}/project/evaluate/`, evaluateRequest)
        .then(a => a.data)
        .then(right)
        .catch(e => Promise.resolve(left(e.response.data)))

export const bindExpression = (
    bindExpressionRequest: BindExpressionRequest
): Promise<Either<string, BindExpressionResponse>> =>
    axios
        .post(`${baseUrl}/project/bind/`, bindExpressionRequest)
        .then(a => a.data)
        .then(right)
        .catch(e => Promise.resolve(left(e.response.data)))

export const bindType = (
    bindTypeRequest: BindTypeRequest
): Promise<Either<string, BindTypeResponse>> =>
    axios
        .post(`${baseUrl}/project/type/`, bindTypeRequest)
        .then(a => a.data)
        .then(right)
        .catch(e => Promise.resolve(left(e.response.data)))

export const addUnitTest = (
    addUnitTestRequest: AddUnitTestRequest
): Promise<Either<string, AddUnitTestResponse>> =>
    axios
        .post(`${baseUrl}/project/tests/add/`, addUnitTestRequest)
        .then(a => a.data)
        .then(right)
        .catch(e => Promise.resolve(left(e.response.data)))

export const listBindings = (
    listBindingsRequest: ListBindingsRequest
): Promise<ListBindingsResponse> =>
    axios
        .post(`${baseUrl}/project/bindings/`, listBindingsRequest)
        .then(a => a.data)

export const createProject = (): Promise<CreateProjectResponse> =>
    axios.get(`${baseUrl}/project/create`).then(a => a.data)

export const getExpression = (
    getExpressionRequest: GetExpressionRequest
): Promise<GetExpressionResponse> =>
    axios
        .post(`${baseUrl}/project/expression/`, getExpressionRequest)
        .then(a => a.data)
