import * as O from 'fp-ts/Option'
import { pipe } from 'fp-ts/function'
import { State } from '../types'
import { ExprHash } from '../../types'
import {
  findExpression,
  findExpressionForAnyBinding,
} from '../project/helpers'
import { Screen } from '../view/screen'

import { editorNew, showBinding } from './feedback'
import { EditorState } from './types'

export const emptyEditor: EditorState = {
  code: '',
  stale: false,
  feedback: editorNew(),
  bindingName: O.none,
}

export const editorForBinding =
  (state: State) =>
  (
    bindingName: string,
    exprHash: ExprHash
  ): O.Option<EditorState> =>
    pipe(
      findExpression(state)(exprHash),
      O.alt(() =>
        findExpressionForAnyBinding(bindingName, state)
      ),
      O.map(({ expression }) => ({
        code: expression.edPretty,
        stale: false,
        feedback: showBinding(expression),
        bindingName: O.some(bindingName),
      }))
    )

export const newEditorFromScreen = (
  screen: Screen
): EditorState =>
  screen.type === 'new-expression' ||
  screen.type === 'edit' ||
  screen.type === 'scratch'
    ? {
        ...emptyEditor,
        code: screen.editor.code,
        stale: true,
      }
    : emptyEditor
