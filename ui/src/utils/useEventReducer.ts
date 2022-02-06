import * as React from 'react'
import { Lens, Optional } from 'monocle-ts'
import * as O from 'fp-ts/Option'
import { pipe } from 'fp-ts/function'
import * as T from 'fp-ts/Task'

type EventReducerResponse<State, Event> = {
  type: 'StateAndEvents'
  state: State
  events: Event[]
}

export const stateOnly = <State, Event>(
  state: State
): EventReducerResponse<State, Event> => ({
  type: 'StateAndEvents',
  state,
  events: [],
})

export const stateAndEvent = <State, Event>(
  state: State,
  event: Event
): EventReducerResponse<State, Event> => ({
  type: 'StateAndEvents',
  state,
  events: [event],
})

export const stateAndEvents = <State, Event>(
  state: State,
  events: Event[]
): EventReducerResponse<State, Event> => ({
  type: 'StateAndEvents',
  state,
  events,
})

export type EventReducer<State, Action, Event> = (
  state: State,
  action: Action
) => EventReducerResponse<State, Event>

export type EventReducerRuntime<State, Action, Event> = (
  state: State,
  event: Event
) => T.Task<Action[]>

export const useEventReducer = <State, Action, Event>(
  reducer: EventReducer<State, Action, Event>,
  initialState: State,
  runtime: EventReducerRuntime<State, Action, Event>
): [State, (a: Action) => void] => {
  const currentAction = React.useRef<Action | null>(null)
  let willBeDispatch = (a: Action) => {
    return
  }

  const [state, dispatch] = React.useReducer(
    (state: State, action: Action) => {
      const response = reducer(state, action)
      if (currentAction.current !== action) {
        response.events.forEach((event) =>
          setTimeout(
            () =>
              runtime(response.state, event)().then(
                (actions) => {
                  actions.forEach(willBeDispatch)
                }
              ),
            1
          )
        )
        currentAction.current = action
      }
      return response.state
    },
    initialState
  )

  // weird hack to allow us to pass dispatch to the runtime
  willBeDispatch = dispatch
  return [state, dispatch]
}

// do reducerA, then reducerB
export const appendReducer =
  <State, A1, A2, E1, E2>(
    reducerA: EventReducer<State, A1, E1>,
    reducerB: EventReducer<State, A2, E2>
  ): EventReducer<State, A1 | A2, E1 | E2> =>
  (state, action) => {
    const responseA = reducerA(state, action as A1)
    const responseB = reducerB(
      responseA.state,
      action as A2
    )
    return {
      type: 'StateAndEvents',
      state: responseB.state,
      events: [
        ...responseA.events,
        ...responseB.events,
      ] as (E1 | E2)[],
    }
  }

// make a small reducer into a big one
export const lensReducer =
  <StateS, StateA, Action, Event>(
    lens: Lens<StateS, StateA>,
    reducer: EventReducer<StateA, Action, Event>
  ): EventReducer<StateS, Action, Event> =>
  (stateS, action) => {
    const stateA = lens.get(stateS)
    const response = reducer(stateA, action)
    return {
      ...response,
      state: lens.set(response.state)(stateS),
    }
  }

// make a small reducer into a big one
export const prismReducer =
  <StateS, StateA, Action, Event>(
    optional: Optional<StateS, StateA>,
    reducer: EventReducer<StateA, Action, Event>
  ): EventReducer<StateS, Action, Event> =>
  (stateS, action) =>
    pipe(
      optional.getOption(stateS),
      O.fold(
        () => stateOnly(stateS),
        (stateA) => {
          const response = reducer(stateA, action)
          return {
            ...response,
            state: optional.set(response.state)(stateS),
          }
        }
      )
    )
