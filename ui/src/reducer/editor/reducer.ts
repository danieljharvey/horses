import * as O from 'fp-ts/Option'

import {
  EventReducer,
  stateOnly,
} from '../../hooks/useEventReducer'
import { ProjectEvent } from '../project/events'
import { EditorState } from './types'

import { EditorAction } from './actions'
import { showPreviewSuccess } from './feedback'
import { pipe } from 'fp-ts/lib/function'
import { getExpressionData } from './selector'

export const editorReducer: EventReducer<
  EditorState,
  EditorAction,
  ProjectEvent
> = (state, action) => {
  switch (action.type) {
    case 'UpdateCode':
      return stateOnly({
        ...state,
        code: action.text,
        stale: true,
      })

    case 'FormatExpression':
      return pipe(
        getExpressionData(state),
        O.fold(
          () => stateOnly(state),
          (expressionData) =>
            stateOnly({
              ...state,
              code: expressionData.edPretty,
            })
        )
      )

    case 'ExpressionPreviewSuccess':
      return stateOnly({
        ...state,
        stale: true,
        feedback: showPreviewSuccess(action.expression),
      })

    default:
      return stateOnly(state)
  }
}
