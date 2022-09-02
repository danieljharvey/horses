import type {
  ExpressionData,
  EvaluateResponse,
  UserErrorResponse,
  TestData,
} from '../../types/'

export const updateCode = (text: string) => ({
  type: 'UpdateCode' as const,
  text,
})
export const formatExpression = () => ({
  type: 'FormatExpression' as const,
})

export const evaluateExpression = (text: string) => ({
  type: 'EvaluateExpression' as const,
  text,
})

export const evaluateExpressionSuccess = (
  expression: EvaluateResponse
) => ({
  type: 'EvaluateExpressionSuccess' as const,
  expression,
})

export const evaluateExpressionFailure = (
  typeError: UserErrorResponse
) => ({
  type: 'EvaluateExpressionFailure' as const,
  typeError,
})

export const addUnitTest = (testName: string) => ({
  type: 'AddUnitTest' as const,
  testName,
})

export const addUnitTestSuccess = (tests: TestData) => ({
  type: 'AddUnitTestSuccess' as const,
  tests,
})

export const addUnitTestFailure = (
  error: UserErrorResponse
) => ({
  type: 'AddUnitTestFailure' as const,
  error,
})

export const bindExpression = (
  bindingName: string,
  code: string,
  updateProject: boolean
) => ({
  type: 'BindExpression' as const,
  bindingName,
  code,
  updateProject,
})

export const bindExpressionSuccess = (
  expression: ExpressionData,
  bindingName: string
) => ({
  type: 'BindExpressionSuccess' as const,
  expression,
  bindingName,
})

export const expressionPreviewSuccess = (
  expression: ExpressionData
) => ({
  type: 'ExpressionPreviewSuccess' as const,
  expression,
})

export const bindExpressionFailure = (
  error: UserErrorResponse
) => ({
  type: 'BindExpressionFailure' as const,
  error,
})

export type EditorAction =
  | ReturnType<typeof updateCode>
  | ReturnType<typeof formatExpression>
  | ReturnType<typeof evaluateExpression>
  | ReturnType<typeof evaluateExpressionSuccess>
  | ReturnType<typeof evaluateExpressionFailure>
  | ReturnType<typeof bindExpression>
  | ReturnType<typeof bindExpressionSuccess>
  | ReturnType<typeof bindExpressionFailure>
  | ReturnType<typeof expressionPreviewSuccess>
