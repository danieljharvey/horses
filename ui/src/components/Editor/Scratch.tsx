import * as React from 'react'
import { Action, EditorState } from '../../reducer/types'
import { CodeEditor } from './CodeEditor'
import { Feedback } from './Feedback'
import { Panel } from '../View/Panel'

import { Button } from '../View/Button'
import { ExprHash } from '../../types'
import { fetchExpressionsForHashes } from '../../reducer/project/actions'

import { flow } from 'fp-ts/function'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import { getSourceItemsFromEditor } from '../../reducer/editor/selector'

type Props = {
  projectHash: ExprHash
  dispatch: (a: Action) => void
  editor: EditorState
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
}

export const Scratch: React.FC<Props> = ({
  dispatch,
  editor,
  onBindingSelect,
  projectHash,
}) => {
  const onEvaluateExpression = () =>
    dispatch({ type: 'EvaluateExpression' })

  const onCodeChange = (text: string) =>
    dispatch({ type: 'UpdateCode', text })

  const onFetchExpressionsForHashes = flow(
    fetchExpressionsForHashes,
    dispatch
  )
  return (
    <>
      <Panel flexGrow={2}>
        <CodeEditor
          code={editor.code}
          sourceItems={getSourceItemsFromEditor(editor)}
          setCode={onCodeChange}
        />
      </Panel>
      <Panel>
        <FlexColumnSpaced>
          {editor.stale && (
            <Button onClick={() => onEvaluateExpression()}>
              Evaluate
            </Button>
          )}
          <Feedback
            result={editor.expression}
            onBindingSelect={onBindingSelect}
            onFetchExpressionsForHashes={
              onFetchExpressionsForHashes
            }
            projectHash={projectHash}
          />
        </FlexColumnSpaced>
      </Panel>
    </>
  )
}
