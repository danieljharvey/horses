import { Option } from 'fp-ts/lib/Option'
import {
  ExpressionData,
  EvaluateResponse,
  UnitTestData,
  UserErrorResponse,
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
  | {
      type: 'ShowErrorResponse'
      errorResponse: UserErrorResponse
    }
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
    }

export type EditorAction =
  | { type: 'UpdateCode'; text: string }
  | { type: 'EvaluateExpression'; text: string }
  | { type: 'FormatExpression' }
  | {
      type: 'EvaluateExpressionSuccess'
      expression: EvaluateResponse
    }
  | {
      type: 'EvaluateExpressionFailure'
      typeError: UserErrorResponse
    }
  | { type: 'EvaluateExpressionError' }
  | { type: 'AddUnitTest'; testName: string }
  | { type: 'AddUnitTestSuccess'; unitTest: UnitTestData }
  | { type: 'AddUnitTestFailure'; error: UserErrorResponse }
  | { type: 'BindExpression'; bindingName: string }
  | {
      type: 'BindExpressionSuccess'
      expression: ExpressionData
      bindingName: string
    }
  | {
      type: 'BindExpressionFailure'
      error: UserErrorResponse
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
