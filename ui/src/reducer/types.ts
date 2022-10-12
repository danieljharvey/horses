import { ViewState, ViewEvent } from './view/types'
import { ViewAction } from './view/actions'
import { ConsoleState, ConsoleEvent } from './console/types'
import { ConsoleAction } from './console/actions'
import { ProjectState } from './project/types'
import { ProjectAction } from './project/actions'
import { ProjectEvent } from './project/events'
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

export type Event = ProjectEvent | ViewEvent | ConsoleEvent
