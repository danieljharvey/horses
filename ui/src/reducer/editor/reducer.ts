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
  EditorEvent,
} from './events'
import { EditorAction } from './actions'
import {
  showErrorResponse,
  showUpdatedBinding,
  showEvaluate,
  showPreviewSuccess,
} from './feedback'
import { pipe } from 'fp-ts/lib/function'
import { getExpressionData } from './selector'

const staleL = Lens.fromProp<EditorState>()('stale')
const codeL = Lens.fromProp<EditorState>()('code')

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
        feedback: showErrorResponse(action.typeError),
      })

    case 'EvaluateExpressionSuccess':
      return stateOnly({
        ...state,
        feedback: showEvaluate(
          action.expression.erExpressionData,
          action.expression.erResult
        ),
        stale: false,
      })
    case 'BindExpression':
      return stateAndEvent(
        pipe(
          state,
          staleL.set(false),
          codeL.set(action.code)
        ),
        doBindExpression(
          action.bindingName,
          action.code,
          action.updateProject
        )
      )
    case 'BindExpressionSuccess':
      return stateOnly({
        ...state,
        code: action.expression.edPretty,
        bindingName: O.some(action.bindingName),
        stale: false,
        feedback: showUpdatedBinding(
          action.expression,
          action.bindingName
        ),
      })
    case 'ExpressionPreviewSuccess':
      return stateOnly({
        ...state,
        stale: true,
        feedback: showPreviewSuccess(action.expression),
      })
    case 'BindExpressionFailure':
      return stateOnly({
        ...state,
        feedback: showErrorResponse(action.error),
      })

    default:
      return stateOnly(state)
  }
}
