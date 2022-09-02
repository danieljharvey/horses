import { EditorState } from '../editor/types'
import { ModuleHash } from '../../types'

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

export const scratchScreen = (editor: EditorState) => ({
  type: 'scratch' as const,
  editor,
})

const scratchModuleScreen = (editor: EditorState) => ({
  type: 'scratch-module' as const,
  editor,
})

export const editModuleScreen = (
  moduleHash: ModuleHash
) => ({
  type: 'edit-module' as const,
  moduleHash,
})

export const typeSearchScreen = () => ({
  type: 'typeSearch' as const,
})

export const newTypeScreen = (editor: EditorState) => ({
  type: 'new-type' as const,
  editor,
})

export type Screen =
  | ReturnType<typeof scratchScreen>
  | ReturnType<typeof editScreen>
  | ReturnType<typeof newExpressionScreen>
  | ReturnType<typeof typeSearchScreen>
  | ReturnType<typeof newTypeScreen>
  | ReturnType<typeof scratchModuleScreen>
  | ReturnType<typeof editModuleScreen>
