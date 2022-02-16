import {
  ExpressionData,
  PropertyTestData,
  TestData,
  UnitTestData,
  UserErrorResponse,
} from '../../types'

export const editorNew = () => ({
  type: 'EditorNew' as const,
})

export const showBinding = (
  expression: ExpressionData
) => ({
  type: 'ShowBinding' as const,
  expression,
})

export const showUpdatedBinding = (
  expression: ExpressionData,
  tests: TestData,
  bindingName: string
) => ({
  type: 'ShowUpdatedBinding' as const,
  expression,
  bindingName,
  tests,
})

export const showTest = (
  test: UnitTestData | PropertyTestData
) => ({
  type: 'ShowTest' as const,
  test,
})

export const showErrorResponse = (
  errorResponse: UserErrorResponse
) => ({
  type: 'ShowErrorResponse' as const,
  errorResponse,
})

export const evaluationError = () => ({
  type: 'EvaluationError' as const,
})

export const showEvaluate = (
  expression: ExpressionData,
  evaluatedValue: string
) => ({
  type: 'ShowEvaluate' as const,
  expression,
  evaluatedValue,
})

export type ExpressionResult =
  | ReturnType<typeof editorNew>
  | ReturnType<typeof showBinding>
  | ReturnType<typeof showErrorResponse>
  | ReturnType<typeof showTest>
  | ReturnType<typeof evaluationError>
  | ReturnType<typeof showEvaluate>
  | ReturnType<typeof showUpdatedBinding>
