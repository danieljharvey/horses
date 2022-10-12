import { Lens } from 'monocle-ts'
import { State } from '../types'
import {
  EventReducer,
  stateOnly,
  stateAndEvent,
  stateAndEvents,
} from '../../hooks/useEventReducer'

import {
  listBindings,
  saveToSessionStorage,
  fetchModule,
  ProjectEvent,
} from './events'
import { projectHash } from '../../types'
import { ProjectAction } from './actions'
import { ProjectState } from './types'

const projectL = Lens.fromProp<State>()('project')

export const projectReducer: EventReducer<
  State,
  ProjectAction,
  ProjectEvent
> = (state, action) => {
  switch (action.type) {
    case 'Initialise':
      // attempt to find project in session storage
      return stateAndEvent(
        state,
        listBindings(state.project.projectHash)
      )

    case 'StoreProjectData':
      // store new bindings, fetching new expressions

      return stateAndEvents(
        projectL.set({
          ...state.project,
          projectHash: projectHash(action.data.pdHash),
          modules: action.data
            .pdModuleBindings as ProjectState['modules'],
        })(state),
        [
          saveToSessionStorage(
            projectHash(action.data.pdHash)
          ),
        ]
      )

    case 'FetchModule':
      return stateAndEvent(
        state,
        fetchModule(action.moduleHash)
      )

    default:
      return stateOnly(state)
  }
}
