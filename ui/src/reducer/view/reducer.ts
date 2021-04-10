import { Lens } from 'monocle-ts'
import { EventReducer, stateOnly } from '../../utils/useEventReducer'
import { Screen, ViewState, ViewAction, ViewEvent } from './types'
import * as NE from 'fp-ts/NonEmptyArray'
import * as O from 'fp-ts/Option'

import { emptyEditor } from '../editor/helpers'

export * from './types'

export const initialView: ViewState = {
    stack: NE.of({ type: 'scratch', editor: emptyEditor }),
}

export const setScreen = (screen: Screen): ViewAction => ({
    type: 'SetScreen',
    screen,
})

export const pushScreen = (screen: Screen): ViewAction => ({
    type: 'PushScreen',
    screen,
})

export const popScreen = (): ViewAction => ({ type: 'PopScreen' })

export const replaceScreen = (screen: Screen): ViewAction => ({
    type: 'ReplaceScreen',
    screen,
})

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
                stackL.modify(stack => NE.cons(action.screen, stack))(
                    state
                )
            )
        case 'PopScreen':
            return stateOnly(
                stackL.modify(stack => {
                    const tail = NE.fromArray(NE.tail(stack))
                    return O.isSome(tail) ? tail.value : stack
                })(state)
            )
        case 'ReplaceScreen':
            return stateOnly(
                stackL.modify(stack => {
                    const tail = NE.tail(stack)
                    return NE.cons(action.screen, tail)
                })(state)
            )
        default:
            return stateOnly(state)
    }
}
