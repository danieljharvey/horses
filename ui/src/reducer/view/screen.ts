import { EditorState } from '../editor/types'
import { ExprHash } from '../../types'

export const editScreen = (
  bindingName: string,
  editor: EditorState
) => ({
  type: 'edit' as const,
  bindingName,
  editor,
})

export const newExpressionScreen = (
  editor: EditorState
) => ({
  type: 'new-expression' as const,
  editor,
})

export const newTestScreen = (editor: EditorState) => ({
  type: 'new-test' as const,
  editor,
})

export const scratchScreen = (editor: EditorState) => ({
  type: 'scratch' as const,
  editor,
})

export const typeSearchScreen = () => ({
  type: 'typeSearch' as const,
})

export const newTypeScreen = (editor: EditorState) => ({
  type: 'new-type' as const,
  editor,
})

export const projectGraphScreen = () => ({
  type: 'project-graph' as const,
})

export const expressionGraphScreen = (
  bindingName: string,
  exprHash: ExprHash
) => ({
  type: 'expression-graph' as const,
  exprHash,
  bindingName,
})

export type Screen =
  | ReturnType<typeof scratchScreen>
  | ReturnType<typeof editScreen>
  | ReturnType<typeof newExpressionScreen>
  | ReturnType<typeof newTestScreen>
  | ReturnType<typeof typeSearchScreen>
  | ReturnType<typeof newTypeScreen>
  | ReturnType<typeof projectGraphScreen>
  | ReturnType<typeof expressionGraphScreen>
