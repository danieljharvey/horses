import { Lens, Optional, Prism } from 'monocle-ts'
import { State } from '../types'
import * as O from 'fp-ts/Option'
import { ExpressionResult, EditorState } from './types'
import { ExpressionData, SourceItem } from '../../types'
import { pipe } from 'fp-ts/function'
import * as NE from 'fp-ts/NonEmptyArray'
import {
  ViewState,
  Screen,
  EditScreen,
  NewExpressionScreen,
  ScratchScreen,
} from '../view/types'

import { viewL } from '../view/selectors'

const neHeadL = <A>(): Lens<NE.NonEmptyArray<A>, A> =>
  new Lens(
    (s) => NE.head(s),
    (a) => (s) => NE.cons(a, NE.tail(s))
  )

type ScreenWithEditor =
  | EditScreen
  | NewExpressionScreen
  | ScratchScreen

const stackL = viewL.compose(
  Lens.fromProp<ViewState>()('stack')
)

const editPrism: Prism<
  Screen,
  ScreenWithEditor
> = new Prism(
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
      updatedTestsCount: number
    }

const expressionDataPrism: Prism<
  ExpressionResult,
  ExpressionDataResult
> = new Prism(
  (res: ExpressionResult) =>
    res.type === 'ShowBinding' ||
    res.type === 'ShowEvaluate' ||
    res.type === 'ShowUpdatedBinding'
      ? O.some(res as ExpressionDataResult)
      : O.none,

  (a: ExpressionDataResult) => a as ExpressionResult
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

const expressionO: Optional<
  EditorState,
  ExpressionData
> = Lens.fromProp<EditorState>()('expression')
  .composePrism(expressionDataPrism)
  .composeLens(expressionDataLens)

const sourceItemsO: Optional<
  EditorState,
  SourceItem[]
> = expressionO.composeLens(
  Lens.fromProp<ExpressionData>()('edSourceItems')
)

const sourceItemsFromState: Optional<
  State,
  SourceItem[]
> = currentEditorO.composeOptional(sourceItemsO)

export const getSourceItems = (
  state: State
): SourceItem[] =>
  pipe(
    sourceItemsFromState.getOption(state),
    O.getOrElse<SourceItem[]>(() => [])
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
): O.Option<ExpressionData> => expressionO.getOption(state)
