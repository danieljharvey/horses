import { Option } from 'fp-ts/lib/Option'
import {
  ExpressionData,
  EvaluateResponse,
  UnitTestData,
} from '../../types/'

export type EditorState = {
  code: string
  stale: boolean
  expression: ExpressionResult
  bindingName: Option<string>
}

export type ExpressionResult =
  | { type: 'EditorNew' }
  | { type: 'ShowBinding'; expression: ExpressionData }
  | { type: 'ShowError'; error: string }
  | { type: 'ShowUnitTest'; unitTest: UnitTestData }
  | { type: 'EvaluationError' }
  | {
      type: 'ShowEvaluate'
      expression: ExpressionData
      evaluatedValue: string
    }
  | {
      type: 'ShowUpdatedBinding'
      bindingName: string
      expression: ExpressionData
      updatedTestsCount: number
    }

export type EditorAction =
  | { type: 'UpdateCode'; text: string }
  | { type: 'EvaluateExpression'; text: string }
  | { type: 'FormatExpression' }
  | {
      type: 'EvaluateExpressionSuccess'
      expression: EvaluateResponse
    }
  | { type: 'EvaluateExpressionFailure'; typeError: string }
  | { type: 'EvaluateExpressionError' }
  | { type: 'AddUnitTest'; testName: string }
  | { type: 'AddUnitTestSuccess'; unitTest: UnitTestData }
  | { type: 'AddUnitTestFailure'; error: string }
  | { type: 'BindExpression'; bindingName: string }
  | {
      type: 'BindExpressionSuccess'
      expression: ExpressionData
      bindingName: string
      updatedTestsCount: number
    }
  | { type: 'BindExpressionFailure'; error: string }

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
