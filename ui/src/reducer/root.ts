import { State, Action, Event } from './types'
import {
  appendReducer,
  EventReducer,
  lensReducer,
  prismReducer,
} from '../utils/useEventReducer'
import { editorReducer } from './editor/reducer'
import {
  initialProject,
  projectReducer,
} from './project/reducer'
import { initialView, viewReducer } from './view/reducer'
import {
  initialConsole,
  consoleReducer,
} from './console/reducer'
import { currentEditorO } from './editor/selector'
import { Lens } from 'monocle-ts'

const viewL = Lens.fromProp<State>()('view')
const consoleL = Lens.fromProp<State>()('console')

export const initialState = (
  projectHash: string
): State => ({
  view: initialView,
  project: initialProject(projectHash),
  console: initialConsole,
})

const viewReducerLensed = lensReducer(viewL, viewReducer)

const consoleReducerLensed = lensReducer(
  consoleL,
  consoleReducer
)

const currentEditorReducer = prismReducer(
  currentEditorO,
  editorReducer
)

export const eventReducer: EventReducer<
  State,
  Action,
  Event
> = appendReducer(
  appendReducer(projectReducer, currentEditorReducer),
  appendReducer(viewReducerLensed, consoleReducerLensed)
)
