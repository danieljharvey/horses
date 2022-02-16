import { Option } from 'fp-ts/lib/Option'
import { ExpressionResult } from './expressionResult'

export type EditorState = {
  code: string
  stale: boolean
  expression: ExpressionResult
  bindingName: Option<string>
}

export type EditorEvent =
  | {
      type: 'EvaluateExpression'
      code: string
    }
  | {
      type: 'BindExpression'
      code: string
      bindingName: string
    }
  | { type: 'AddUnitTest'; testName: string; code: string }
  | { type: 'UpgradeExpression'; bindingName: string }
  | { type: 'OptimiseExpression'; bindingName: string }
