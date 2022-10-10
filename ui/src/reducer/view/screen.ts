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

export const scratchModuleScreen = (
  editor: EditorState
) => ({
  type: 'scratch-module' as const,
  editor,
})

export const editModuleScreen = (
  moduleHash: ModuleHash
) => ({
  type: 'edit-module' as const,
  moduleHash,
})

export type Screen =
  | ReturnType<typeof editScreen>
  | ReturnType<typeof scratchModuleScreen>
  | ReturnType<typeof editModuleScreen>
