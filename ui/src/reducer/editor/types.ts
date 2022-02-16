import { Option } from 'fp-ts/lib/Option'
import { ExpressionResult } from './expressionResult'

export type EditorState = {
  code: string
  stale: boolean
  expression: ExpressionResult
  bindingName: Option<string>
}
