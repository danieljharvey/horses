import type { ExpressionData } from '../../types/'

export const updateCode = (text: string) => ({
  type: 'UpdateCode' as const,
  text,
})
export const formatExpression = () => ({
  type: 'FormatExpression' as const,
})

export const expressionPreviewSuccess = (
  expression: ExpressionData
) => ({
  type: 'ExpressionPreviewSuccess' as const,
  expression,
})

export type EditorAction =
  | ReturnType<typeof updateCode>
  | ReturnType<typeof formatExpression>
  | ReturnType<typeof expressionPreviewSuccess>
