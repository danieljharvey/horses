import { Lens, Optional, Prism } from 'monocle-ts'
import { State } from '../types'
import * as O from 'fp-ts/Option'
import { EditorState } from './types'

import {
  ExpressionData,
  SourceItem,
  UserErrorResponse,
  TypedHoleResponse,
  ErrorLocation,
  TestData,
} from '../../types'
import { pipe } from 'fp-ts/function'
import * as NE from 'fp-ts/NonEmptyArray'
import { ViewState } from '../view/types'
import {
  Screen,
  editScreen,
  newExpressionScreen,
  scratchScreen,
} from '../view/screen'
import { viewL } from '../view/selectors'
import { Feedback } from './feedback'

const neHeadL = <A>(): Lens<NE.NonEmptyArray<A>, A> =>
  new Lens(
    (s) => NE.head(s),
    (a) => (s) => NE.cons(a, NE.tail(s))
  )

type ScreenWithEditor =
  | ReturnType<typeof editScreen>
  | ReturnType<typeof newExpressionScreen>
  | ReturnType<typeof scratchScreen>

const stackL = viewL.compose(
  Lens.fromProp<ViewState>()('stack')
)

const editPrism: Prism<Screen, ScreenWithEditor> =
  new Prism(
    (s: Screen) =>
      s.type === 'edit' ||
      s.type === 'new-expression' ||
      s.type === 'scratch' ||
      s.type === 'scratch-module' ||
      s.type === 'new-type'
        ? O.some(s as ScreenWithEditor)
        : O.none,
    (a: ScreenWithEditor) => a as Screen
  )

const editorFromScreen: Lens<
  ScreenWithEditor,
  EditorState
> = new Lens(
  (s) => s.editor,
  (a) => (s) => ({ ...s, editor: a })
)

export const currentEditorO = stackL
  .composeLens(neHeadL<Screen>())
  .composePrism(editPrism)
  .composeLens(editorFromScreen)

type ExpressionDataResult =
  | { type: 'ShowBinding'; expression: ExpressionData }
  | {
      type: 'ShowEvaluate'
      expression: ExpressionData
      evaluatedValue: string
    }
  | {
      type: 'ShowUpdatedBinding'
      bindingName: string
      expression: ExpressionData
      tests: TestData
    }

const expressionDataPrism: Prism<
  Feedback,
  ExpressionDataResult
> = new Prism(
  (res: Feedback) =>
    res.type === 'ShowBinding' ||
    res.type === 'ShowEvaluate' ||
    res.type === 'ShowUpdatedBinding'
      ? O.some(res as ExpressionDataResult)
      : O.none,

  (a: ExpressionDataResult) => a as Feedback
)

const expressionDataLens: Lens<
  ExpressionDataResult,
  ExpressionData
> = new Lens(
  (res: ExpressionDataResult) => {
    switch (res.type) {
      case 'ShowUpdatedBinding':
        return res.expression
      case 'ShowEvaluate':
        return res.expression
      case 'ShowBinding':
        return res.expression
    }
  },
  (expr) => (s) => ({ ...s, expression: expr })
)

type UserErrorResult = {
  type: 'ShowErrorResponse'
  errorResponse: UserErrorResponse
}

const userErrorResponsePrism: Prism<
  Feedback,
  UserErrorResult
> = new Prism(
  (res: Feedback) =>
    res.type === 'ShowErrorResponse'
      ? O.some(res as UserErrorResult)
      : O.none,
  (a: UserErrorResult) => a as Feedback
)

const errorResponseO: Optional<
  EditorState,
  UserErrorResponse
> = Lens.fromProp<EditorState>()('feedback')
  .composePrism(userErrorResponsePrism)
  .composeLens(
    Lens.fromProp<UserErrorResult>()('errorResponse')
  )

const typedHolesO: Optional<
  EditorState,
  TypedHoleResponse[]
> = errorResponseO.composeLens(
  Lens.fromProp<UserErrorResponse>()('ueTypedHoles')
)

const errorLocationsO: Optional<
  EditorState,
  ErrorLocation[]
> = errorResponseO.composeLens(
  Lens.fromProp<UserErrorResponse>()('ueErrorLocations')
)

const feedbackO: Optional<EditorState, ExpressionData> =
  Lens.fromProp<EditorState>()('feedback')
    .composePrism(expressionDataPrism)
    .composeLens(expressionDataLens)

const sourceItemsO: Optional<EditorState, SourceItem[]> =
  feedbackO.composeLens(
    Lens.fromProp<ExpressionData>()('edSourceItems')
  )

const sourceItemsFromState: Optional<State, SourceItem[]> =
  currentEditorO.composeOptional(sourceItemsO)

export const getSourceItems = (
  state: State
): SourceItem[] =>
  pipe(
    sourceItemsFromState.getOption(state),
    O.getOrElse<SourceItem[]>(() => [])
  )

export const getTypedHolesFromEditor = (
  editorState: EditorState
): TypedHoleResponse[] =>
  pipe(
    typedHolesO.getOption(editorState),
    O.getOrElse<TypedHoleResponse[]>(() => [])
  )

export const getTypedHoles = (
  state: State
): TypedHoleResponse[] =>
  pipe(
    currentEditorO
      .composeOptional(typedHolesO)
      .getOption(state),
    O.getOrElse<TypedHoleResponse[]>(() => [])
  )

export const getErrorLocationsFromEditor = (
  editorState: EditorState
): ErrorLocation[] =>
  pipe(
    errorLocationsO.getOption(editorState),
    O.getOrElse<ErrorLocation[]>(() => [])
  )

export const getErrorLocations = (
  state: State
): ErrorLocation[] =>
  pipe(
    currentEditorO
      .composeOptional(errorLocationsO)
      .getOption(state),
    O.getOrElse<ErrorLocation[]>(() => [])
  )

export const getSourceItemsFromEditor = (
  state: EditorState
): SourceItem[] =>
  pipe(
    sourceItemsO.getOption(state),
    O.getOrElse<SourceItem[]>(() => [])
  )

export const getExpressionData = (
  state: EditorState
): O.Option<ExpressionData> => feedbackO.getOption(state)
