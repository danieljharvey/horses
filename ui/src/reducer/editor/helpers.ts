import * as O from 'fp-ts/Option'
import { pipe } from 'fp-ts/function'
import { State, EditorState } from '../types'
import {
  findExpression,
  findExpressionForAnyBinding,
} from '../project/helpers'
import { Screen } from '../view/types'
import { showBinding } from './reducer'

export const emptyEditor: EditorState = {
  code: '',
  stale: false,
  expression: { type: 'EditorNew' },
  bindingName: O.none,
}

export const editorForBinding = (
  bindingName: string,
  exprHash: string,
  state: State
): O.Option<EditorState> =>
  pipe(
    findExpression(exprHash, state),
    O.alt(() =>
      findExpressionForAnyBinding(bindingName, state)
    ),
    O.map(({ expression, tests }) => ({
      code: expression.edPretty,
      stale: false,
      expression: showBinding(expression, tests),
      bindingName: O.some(bindingName),
    }))
  )

export const newEditorFromScreen = (
  screen: Screen
): EditorState =>
  screen.type === 'new-expression' ||
  screen.type === 'new-test' ||
  screen.type === 'edit' ||
  screen.type === 'scratch'
    ? {
        ...emptyEditor,
        code: screen.editor.code,
        stale: true,
      }
    : emptyEditor
