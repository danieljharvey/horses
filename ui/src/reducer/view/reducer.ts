import { Lens } from 'monocle-ts'
import {
  EventReducer,
  stateOnly,
} from '../../hooks/useEventReducer'
import { ViewState, ViewEvent } from './types'
import { ViewAction } from './actions'
import * as NE from 'fp-ts/NonEmptyArray'
import * as O from 'fp-ts/Option'

import { emptyEditor } from '../editor/helpers'
import { scratchModuleScreen } from './screen'

// for now we start with the scratch screen
// this should be changed to `scratchModuleScreen` when we are ready
export const initialView: ViewState = {
  stack: NE.of(scratchModuleScreen(emptyEditor)),
}

const stackL = Lens.fromProp<ViewState>()('stack')

export const viewReducer: EventReducer<
  ViewState,
  ViewAction,
  ViewEvent
> = (state, action) => {
  switch (action.type) {
    case 'SetScreen':
      return stateOnly(stackL.set([action.screen])(state))
    case 'PushScreen':
      return stateOnly(
        stackL.modify((stack) =>
          NE.cons(action.screen, stack)
        )(state)
      )
    case 'PopScreen':
      return stateOnly(
        stackL.modify((stack) => {
          const tail = NE.fromArray(NE.tail(stack))
          return O.isSome(tail) ? tail.value : stack
        })(state)
      )
    default:
      return stateOnly(state)
  }
}
