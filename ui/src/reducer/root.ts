import { State, Action, Event } from './types'
import * as NE from 'fp-ts/NonEmptyArray'
import * as O from 'fp-ts/Option'
import {
    appendReducer,
    EventReducer,
    lensReducer,
    prismReducer,
} from '../utils/useEventReducer'
import { editorReducer, EditorState } from './editor/reducer'
import { initialProject, projectReducer } from './project/reducer'
import {
    initialView,
    viewReducer,
    ViewState,
    Screen,
    EditScreen,
    NewExpressionScreen,
    ScratchScreen,
} from './view/reducer'
import { initialConsole, consoleReducer } from './console/reducer'

import { Lens, Prism } from 'monocle-ts'

export const initialState = (projectHash: string): State => ({
    view: initialView,
    project: initialProject(projectHash),
    console: initialConsole,
})

const viewL = Lens.fromProp<State>()('view')
const viewReducerLensed = lensReducer(viewL, viewReducer)

const consoleL = Lens.fromProp<State>()('console')
const consoleReducerLensed = lensReducer(consoleL, consoleReducer)

const neHeadL = <A>(): Lens<NE.NonEmptyArray<A>, A> =>
    new Lens(
        s => NE.head(s),
        a => s => NE.cons(a, NE.tail(s))
    )

type ScreenWithEditor =
    | EditScreen
    | NewExpressionScreen
    | ScratchScreen

const stackL = viewL.compose(Lens.fromProp<ViewState>()('stack'))

const editPrism: Prism<Screen, ScreenWithEditor> = new Prism(
    (s: Screen) =>
        s.type === 'edit' ||
        s.type === 'new-expression' ||
        s.type === 'scratch' ||
        s.type === 'new-test' ||
        s.type === 'new-type'
            ? O.some(s as ScreenWithEditor)
            : O.none,
    (a: ScreenWithEditor) => a as Screen
)

const editorFromScreen: Lens<
    ScreenWithEditor,
    EditorState
> = new Lens(
    s => s.editor,
    a => s => ({ ...s, editor: a })
)

export const currentEditorO = stackL
    .composeLens(neHeadL<Screen>())
    .composePrism(editPrism)
    .composeLens(editorFromScreen)

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
