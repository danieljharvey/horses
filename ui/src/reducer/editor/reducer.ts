import {
  EventReducer,
  stateOnly,
} from '../../hooks/useEventReducer'
import { ProjectEvent } from '../project/events'
import { EditorState } from './types'

import { EditorAction } from './actions'

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

    default:
      return stateOnly(state)
  }
}
