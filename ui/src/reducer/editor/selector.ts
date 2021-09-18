import { Lens, Optional, Prism } from 'monocle-ts'
import { State } from '../types'
import { currentEditorO } from '../root'
import * as O from 'fp-ts/Option'
import { ExpressionResult, EditorState } from './types'
import { ExpressionData, SourceItem } from '../../types'
import { pipe } from 'fp-ts/function'
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
  expr => s => ({ ...s, expression: expr })
)

const sourceItemsO: Optional<
  EditorState,
  SourceItem[]
> = Lens.fromProp<EditorState>()('expression')
  .composePrism(expressionDataPrism)
  .composeLens(expressionDataLens)
  .composeLens(
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
