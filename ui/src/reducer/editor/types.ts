import { Option } from 'fp-ts/lib/Option'
import { Feedback } from './feedback'

export type EditorState = {
  code: string
  stale: boolean
  feedback: Feedback
  bindingName: Option<string>
}
