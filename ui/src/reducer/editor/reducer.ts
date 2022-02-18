import { Lens } from 'monocle-ts'
import * as O from 'fp-ts/Option'

import {
  EventReducer,
  stateOnly,
  stateAndEvent,
} from '../../hooks/useEventReducer'
import { ProjectEvent } from '../project/events'
import { EditorState } from './types'

import {
  doBindExpression,
  doEvaluateExpression,
  doAddUnitTest,
  EditorEvent,
  doUpgradeExpression,
  doOptimiseExpression,
} from './events'
import { EditorAction } from './actions'
import {
  showErrorResponse,
  showUpdatedBinding,
  showTest,
  showEvaluate,
} from './expressionResult'
import { pipe } from 'fp-ts/lib/function'
import { getExpressionData } from './selector'

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
        doEvaluateExpression(action.text)
      )
    case 'FormatExpression':
      return pipe(
        getExpressionData(state),
        O.fold(
          () => stateOnly(state),
          (expressionData) =>
            stateAndEvent(
              { ...state, code: expressionData.edPretty },
              doEvaluateExpression(expressionData.edPretty)
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
    case 'BindExpression':
      return stateAndEvent(
        staleL.set(false)(state),
        doBindExpression(action.bindingName, state.code)
      )
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
      return stateAndEvent(
        staleL.set(false)(state),
        doAddUnitTest(action.testName, state.code)
      )
    case 'AddUnitTestSuccess':
      const firstTest =
        action.tests.tdUnitTests.find((a) => a) ||
        action.tests.tdPropertyTests.find((a) => a)
      return firstTest
        ? stateOnly({
            ...state,
            state: false,
            expression: showTest(firstTest),
          })
        : stateOnly(state)

    case 'AddUnitTestFailure':
      return stateOnly({
        ...state,
        expression: showErrorResponse(action.error),
      })

    case 'UpgradeExpression':
      return stateAndEvent(
        staleL.set(false)(state),
        doUpgradeExpression(action.bindingName)
      )
    case 'UpgradeExpressionSuccess':
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
    case 'UpgradeExpressionFailure':
      return stateOnly({
        ...state,
        expression: showErrorResponse(action.error),
      })

    case 'OptimiseExpression':
      return stateAndEvent(
        staleL.set(false)(state),
        doOptimiseExpression(action.bindingName)
      )

    case 'OptimiseExpressionSuccess':
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
    case 'OptimiseExpressionFailure':
      return stateOnly({
        ...state,
        expression: showErrorResponse(action.error),
      })

    default:
      return stateOnly(state)
  }
}
