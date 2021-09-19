import * as React from 'react'
import { State, Action } from '../../reducer/types'
import { CodeEditor } from './CodeEditor'
import { EditorState } from '../../reducer/editor/types'
import { Feedback } from './Feedback'
import * as O from 'fp-ts/Option'
import { Panel } from '../View/Panel'
import { Button } from '../View/Button'
import { ExprHash } from '../../types'
import { fetchExpressionsForHashes } from '../../reducer/project/actions'
import { pipe } from 'fp-ts/lib/function'
import { getSourceItems } from '../../reducer/editor/selector'

type Props = {
  state: State
  dispatch: (a: Action) => void
  editor: EditorState
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
}

export const EditBinding: React.FC<Props> = ({
  dispatch,
  editor,
  onBindingSelect,
  state,
}) => {
  const code = editor.code

  const onCodeChange = (a: string) =>
    dispatch({ type: 'UpdateCode', text: a })

  const { expression, stale } = editor

  const bindingName = O.toNullable(editor.bindingName)

  const onBindExpression = () => {
    const bindingName = O.toNullable(editor.bindingName)
    if (bindingName) {
      dispatch({ type: 'BindExpression', bindingName })
    }
  }

  const onFetchExpressionsForHashes = (
    hashes: ExprHash[]
  ) => pipe(hashes, fetchExpressionsForHashes, dispatch)

  return (
    <>
      <Panel flexGrow={2}>
        <CodeEditor
          code={code}
          setCode={onCodeChange}
          sourceItems={getSourceItems(state)}
        />
      </Panel>
      <Panel>
        {stale && (
          <Button onClick={() => onBindExpression()}>
            {`Update ${bindingName}`}
          </Button>
        )}
        <Feedback
          result={expression}
          onBindingSelect={onBindingSelect}
          onFetchExpressionsForHashes={
            onFetchExpressionsForHashes
          }
          projectHash={state.project.projectHash}
        />
      </Panel>
    </>
  )
}
