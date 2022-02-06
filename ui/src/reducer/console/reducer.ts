import { Lens } from 'monocle-ts'
import {
  EventReducer,
  stateOnly,
} from '../../utils/useEventReducer'
import {
  ConsoleState,
  ConsoleAction,
  ConsoleEvent,
  Log,
} from './types'
export type {
  ConsoleState,
  ConsoleAction,
  ConsoleEvent,
} from './types'

export const initialConsole: ConsoleState = {
  logs: [],
}

export const log = (
  message: string,
  timestamp: number
): ConsoleAction => ({
  type: 'Log',
  message,
  timestamp,
})

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
