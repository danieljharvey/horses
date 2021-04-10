import { EditorAction, EditorEvent } from './editor/reducer'
import {
    ProjectState,
    ProjectAction,
    ProjectEvent,
} from './project/reducer'
import { ViewState, ViewAction, ViewEvent } from './view/reducer'
import {
    ConsoleState,
    ConsoleAction,
    ConsoleEvent,
} from './console/reducer'

export * from './view/types'
export * from './console/types'
export * from './project/types'
export * from './editor/types'

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
