import {
  ExpressionData,
  EvaluateResponse,
  UserErrorResponse,
  TestData,
} from '../../types/'

export const updateCode = (text: string) => ({
  type: 'UpdateCode' as const,
  text,
})

export const evaluateExpression = (text: string) => ({
  type: 'EvaluateExpression' as const,
  text,
})

export const formatExpression = () => ({
  type: 'FormatExpression' as const,
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

export const bindExpression = (bindingName: string) => ({
  type: 'BindExpression' as const,
  bindingName,
})

export const bindExpressionSuccess = (
  expression: ExpressionData,
  bindingName: string,
  tests: TestData
) => ({
  type: 'BindExpressionSuccess' as const,
  expression,
  bindingName,
  tests,
})

export const bindExpressionFailure = (
  error: UserErrorResponse
) => ({
  type: 'BindExpressionFailure' as const,
  error,
})

export const upgradeExpression = (bindingName: string) => ({
  type: 'UpgradeExpression' as const,
  bindingName,
})

export const upgradeExpressionSuccess = (
  expression: ExpressionData,
  bindingName: string,
  tests: TestData
) => ({
  type: 'UpgradeExpressionSuccess' as const,
  expression,
  bindingName,
  tests,
})

export const upgradeExpressionFailure = (
  error: UserErrorResponse
) => ({
  type: 'UpgradeExpressionFailure' as const,
  error,
})

export const optimiseExpression = (
  bindingName: string
) => ({
  type: 'OptimiseExpression' as const,
  bindingName,
})

export const optimiseExpressionSuccess = (
  expression: ExpressionData,
  bindingName: string,
  tests: TestData
) => ({
  type: 'OptimiseExpressionSuccess' as const,
  expression,
  bindingName,
  tests,
})

export const optimiseExpressionFailure = (
  error: UserErrorResponse
) => ({
  type: 'OptimiseExpressionFailure' as const,
  error,
})

export type EditorAction =
  | ReturnType<typeof updateCode>
  | ReturnType<typeof formatExpression>
  | ReturnType<typeof evaluateExpression>
  | ReturnType<typeof evaluateExpressionSuccess>
  | ReturnType<typeof evaluateExpressionFailure>
  | ReturnType<typeof addUnitTest>
  | ReturnType<typeof addUnitTestSuccess>
  | ReturnType<typeof addUnitTestFailure>
  | ReturnType<typeof bindExpression>
  | ReturnType<typeof bindExpressionSuccess>
  | ReturnType<typeof bindExpressionFailure>
  | ReturnType<typeof upgradeExpression>
  | ReturnType<typeof upgradeExpressionSuccess>
  | ReturnType<typeof upgradeExpressionFailure>
  | ReturnType<typeof optimiseExpression>
  | ReturnType<typeof optimiseExpressionSuccess>
  | ReturnType<typeof optimiseExpressionFailure>
