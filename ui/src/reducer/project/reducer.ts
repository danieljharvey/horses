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
  fetchExpressions,
  saveToSessionStorage,
  fetchModule,
  ProjectEvent,
} from './events'
import {
  exprHash,
  projectHash,
  ExprHash,
  ProjectData,
} from '../../types'
import { ProjectAction } from './actions'
import { ProjectState } from './types'

const projectL = Lens.fromProp<State>()('project')

// all hashes mentioned in project, for fetching
const hashesForProjectData = (
  projectData: ProjectData
): ExprHash[] =>
  [
    ...Object.values(projectData.pdBindings),
    ...Object.values(projectData.pdTypeBindings),
    ...Object.values(projectData.pdVersions).flatMap(
      (bvs) => bvs.map((bv) => bv.bvExprHash)
    ),
  ].map(exprHash)

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
          bindings: action.data
            .pdBindings as ProjectState['bindings'],
          typeBindings: action.data
            .pdTypeBindings as ProjectState['typeBindings'],
          modules: action.data
            .pdModuleBindings as ProjectState['modules'],
          versions: action.data.pdVersions,
        })(state),
        [
          fetchExpressions(
            [
              ...hashesForProjectData(action.data),
              ...action.extraHashes,
            ],
            projectHash(action.data.pdHash)
          ),
          saveToSessionStorage(
            projectHash(action.data.pdHash)
          ),
        ]
      )

    case 'FetchExpressionsSuccess': {
      const newStore = Object.entries(
        action.fetched
      ).reduce(
        (all, [key, expression]) => ({
          ...all,
          [key]: { expression },
        }),
        {}
      )
      const allExprHashes = Object.values(action.fetched)
        .flatMap(({ edBindings, edTypeBindings }) => [
          ...Object.values(edBindings),
          ...Object.values(edTypeBindings),
        ])
        .map(exprHash)

      return stateAndEvent(
        projectL.set({
          ...state.project,
          store: {
            ...state.project.store,
            ...newStore,
          },
        })(state),
        fetchExpressions(
          allExprHashes,
          state.project.projectHash
        )
      )
    }

    case 'FetchModule':
      return stateAndEvent(
        state,
        fetchModule(action.moduleHash)
      )

    default:
      return stateOnly(state)
  }
}
