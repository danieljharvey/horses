import * as O from 'fp-ts/Option'
import { Screen } from '../view/screen'

import { editorNew } from './feedback'
import { EditorState } from './types'

export const emptyEditor: EditorState = {
  code: '',
  stale: false,
  feedback: editorNew(),
  bindingName: O.none,
}

export const newEditorFromScreen = (
  screen: Screen
): EditorState =>
  screen.type === 'scratch-module'
    ? {
        ...emptyEditor,
        code: screen.editor.code,
        stale: true,
      }
    : emptyEditor
