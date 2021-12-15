import { Lens } from 'monocle-ts'
import * as O from 'fp-ts/Option'
import {
  UnitTestData,
  ExpressionData,
  UserErrorResponse,
  TestData,
} from '../../types'
import {
  EventReducer,
  stateOnly,
  stateAndEvent,
} from '../../utils/useEventReducer'
import { ProjectEvent } from '../project/reducer'
import {
  EditorState,
  ExpressionResult,
  EditorAction,
  EditorEvent,
} from './types'
import { pipe } from 'fp-ts/lib/function'
import { getExpressionData } from './selector'
export * from './types'

export const editorNew = (): ExpressionResult => ({
  type: 'EditorNew',
})

export const showBinding = (
  expression: ExpressionData,
  tests: TestData
): ExpressionResult => ({
  type: 'ShowBinding',
  expression,
  tests,
})

export const showUpdatedBinding = (
  expression: ExpressionData,
  tests: TestData,
  bindingName: string
): ExpressionResult => ({
  type: 'ShowUpdatedBinding',
  expression,
  bindingName,
  tests,
})

export const showUnitTest = (
  unitTest: UnitTestData
): ExpressionResult => ({
  type: 'ShowUnitTest',
  unitTest,
})

const showErrorResponse = (
  errorResponse: UserErrorResponse
): ExpressionResult => ({
  type: 'ShowErrorResponse',
  errorResponse,
})

const evaluationError = (): ExpressionResult => ({
  type: 'EvaluationError',
})

const showEvaluate = (
  expression: ExpressionData,
  evaluatedValue: string
): ExpressionResult => ({
  type: 'ShowEvaluate',
  expression,
  evaluatedValue,
})

const staleL = Lens.fromProp<EditorState>()('stale')

export const editorReducer: EventReducer<
  EditorState,
  EditorAction,
  EditorEvent | ProjectEvent
> = (state, action) => {
  switch (action.type) {
    case 'UpdateCode':
      return stateOnly({
        ...state,
        code: action.text,
        stale: true,
      })
    case 'EvaluateExpression':
      return stateAndEvent(
        {
          ...state,
          code: action.text,
          stale: true,
        },
        {
          type: 'EvaluateExpression',
          code: action.text,
        }
      )
    case 'FormatExpression':
      return pipe(
        getExpressionData(state),
        O.fold(
          () => stateOnly(state),
          (expressionData) =>
            stateAndEvent(
              { ...state, code: expressionData.edPretty },
              {
                type: 'EvaluateExpression',
                code: expressionData.edPretty,
              }
            )
        )
      )

    case 'EvaluateExpressionFailure':
      return stateOnly({
        ...state,
        expression: showErrorResponse(action.typeError),
      })

    case 'EvaluateExpressionSuccess':
      return stateOnly({
        ...state,
        expression: showEvaluate(
          action.expression.erExpressionData,
          action.expression.erResult
        ),
        stale: false,
      })
    case 'EvaluateExpressionError':
      return stateOnly({
        ...state,
        expression: evaluationError(),
      })
    case 'BindExpression':
      return stateAndEvent(staleL.set(false)(state), {
        type: 'BindExpression',
        code: state.code,
        bindingName: action.bindingName,
      })
    case 'BindExpressionSuccess':
      return stateOnly({
        ...state,
        code: action.expression.edPretty,
        bindingName: O.some(action.bindingName),
        stale: false,
        expression: showUpdatedBinding(
          action.expression,
          action.tests,
          action.bindingName
        ),
      })
    case 'BindExpressionFailure':
      return stateOnly({
        ...state,
        expression: showErrorResponse(action.error),
      })
    case 'AddUnitTest':
      return stateAndEvent(staleL.set(false)(state), {
        type: 'AddUnitTest',
        testName: action.testName,
        code: state.code,
      })
    case 'AddUnitTestSuccess':
      return stateOnly({
        ...state,
        state: false,
        expression: showUnitTest(action.unitTest),
      })
    case 'AddUnitTestFailure':
      return stateOnly({
        ...state,
        expression: showErrorResponse(action.error),
      })
    default:
      return stateOnly(state)
  }
}
