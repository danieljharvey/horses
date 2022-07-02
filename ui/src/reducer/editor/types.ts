import { Option } from 'fp-ts/lib/Option'
import * as O from 'fp-ts/lib/Option'
import { Feedback, editorNew } from './feedback'

export const emptyEditor: EditorState = {
  code: '',
  stale: false,
  feedback: editorNew(),
  bindingName: O.none,
}

export type EditorState = {
  code: string
  stale: boolean
  feedback: Feedback
  bindingName: Option<string>
}
