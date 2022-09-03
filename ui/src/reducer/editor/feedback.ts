import type {
  ExpressionData,
  TestData,
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
  bindingName: string
) => ({
  type: 'ShowUpdatedBinding' as const,
  expression,
  bindingName,
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

export const showModule = (
  moduleData: ModuleData,
  testData: TestData
) => ({
  type: 'ShowModuleData' as const,
  moduleData,
  testData,
})

export type Feedback =
  | ReturnType<typeof editorNew>
  | ReturnType<typeof showBinding>
  | ReturnType<typeof showErrorResponse>
  | ReturnType<typeof showEvaluate>
  | ReturnType<typeof showUpdatedBinding>
  | ReturnType<typeof showPreviewSuccess>
  | ReturnType<typeof showModule>
