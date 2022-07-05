import type {
  ExpressionData,
  PropertyTestData,
  TestData,
  UnitTestData,
  UserErrorResponse,
  ModuleData,
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

export const showEvaluate = (
  expression: ExpressionData,
  evaluatedValue: string
) => ({
  type: 'ShowEvaluate' as const,
  expression,
  evaluatedValue,
})

export const showPreviewSuccess = (
  expression: ExpressionData
) => ({
  type: 'ShowPreviewSuccess' as const,
  expression,
})

export const showModule = (moduleData: ModuleData) => ({
  type: 'ShowModuleData' as const,
  moduleData,
})

export type Feedback =
  | ReturnType<typeof editorNew>
  | ReturnType<typeof showBinding>
  | ReturnType<typeof showErrorResponse>
  | ReturnType<typeof showTest>
  | ReturnType<typeof showEvaluate>
  | ReturnType<typeof showUpdatedBinding>
  | ReturnType<typeof showPreviewSuccess>
  | ReturnType<typeof showModule>
