import { Lens } from 'monocle-ts'
import {
  EventReducer,
  stateOnly,
} from '../../utils/useEventReducer'
import { ConsoleState, ConsoleEvent, Log } from './types'
import { ConsoleAction } from './actions'

export const initialConsole: ConsoleState = {
  logs: [],
}

const logL = Lens.fromProp<ConsoleState>()('logs')

export const consoleReducer: EventReducer<
  ConsoleState,
  ConsoleAction,
  ConsoleEvent
> = (state, action) => {
  switch (action.type) {
    case 'Log':
      const log: Log = {
        message: action.message,
        timestamp: action.timestamp,
      }
      return stateOnly(
        logL.modify((logs) => [...logs, log])(state)
      )
    default:
      return stateOnly(state)
  }
}
