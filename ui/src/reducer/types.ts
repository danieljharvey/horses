import {
  ViewState,
  ViewAction,
  ViewEvent,
} from './view/reducer'
import {
  ConsoleState,
  ConsoleAction,
  ConsoleEvent,
} from './console/reducer'
import { ProjectState } from './project/types'
import { ProjectAction } from './project/actions'
import { ProjectEvent } from './project/events'
import { EditorEvent } from './editor/types'
import { EditorAction } from './editor/actions'

export type State = {
  view: ViewState
  project: ProjectState
  console: ConsoleState
}

export type Action =
  | ProjectAction
  | EditorAction
  | ViewAction
  | ConsoleAction

export type Event =
  | ProjectEvent
  | EditorEvent
  | ViewEvent
  | ConsoleEvent
