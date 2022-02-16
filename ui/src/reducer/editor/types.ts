import { Option } from 'fp-ts/lib/Option'
import {
  ExpressionData,
  EvaluateResponse,
  UserErrorResponse,
  TestData,
} from '../../types/'
import { ExpressionResult } from './expressionResult'

export type EditorState = {
  code: string
  stale: boolean
  expression: ExpressionResult
  bindingName: Option<string>
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
  | { type: 'AddUnitTestSuccess'; tests: TestData }
  | { type: 'AddUnitTestFailure'; error: UserErrorResponse }
  | { type: 'BindExpression'; bindingName: string }
  | {
      type: 'BindExpressionSuccess'
      expression: ExpressionData
      bindingName: string
      tests: TestData
    }
  | {
      type: 'BindExpressionFailure'
      error: UserErrorResponse
    }
  | { type: 'UpgradeExpression'; bindingName: string }
  | {
      type: 'UpgradeExpressionSuccess'
      expression: ExpressionData
      bindingName: string
      tests: TestData
    }
  | {
      type: 'UpgradeExpressionFailure'
      error: UserErrorResponse
    }
  | { type: 'OptimiseExpression'; bindingName: string }
  | {
      type: 'OptimiseExpressionSuccess'
      expression: ExpressionData
      bindingName: string
      tests: TestData
    }
  | {
      type: 'OptimiseExpressionFailure'
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
  | { type: 'UpgradeExpression'; bindingName: string }
  | { type: 'OptimiseExpression'; bindingName: string }
