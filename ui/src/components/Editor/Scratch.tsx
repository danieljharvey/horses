import * as React from 'react'
import {
  Action,
  EditorState,
  State,
} from '../../reducer/types'
import { CodeEditor } from './CodeEditor'
import { Feedback } from './Feedback'
import { Panel } from '../View/Panel'

import { Button } from '../View/Button'
import { ExprHash } from '../../types'
import { fetchExpressionsForHashes } from '../../reducer/project/actions'

import { flow } from 'fp-ts/function'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import {
  getTypedHolesFromEditor,
  getSourceItemsFromEditor,
  getErrorLocationsFromEditor,
} from '../../reducer/editor/selector'

type Props = {
  projectHash: ExprHash
  dispatch: (a: Action) => void
  editor: EditorState
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
  state: State
}

export const Scratch: React.FC<Props> = ({
  dispatch,
  editor,
  onBindingSelect,
  projectHash,
  state,
}) => {
  const onFormatExpression = () =>
    dispatch({ type: 'FormatExpression' })

  const onCodeChange = (text: string) =>
    dispatch({ type: 'EvaluateExpression', text })

  const onFetchExpressionsForHashes = flow(
    fetchExpressionsForHashes,
    dispatch
  )

  const typedHoleResponses = getTypedHolesFromEditor(editor)
  const errorLocations = getErrorLocationsFromEditor(editor)

  return (
    <>
      <Panel flexGrow={2}>
        <CodeEditor
          code={editor.code}
          sourceItems={getSourceItemsFromEditor(editor)}
          setCode={onCodeChange}
          errorLocations={errorLocations}
          typedHoleResponses={typedHoleResponses}
        />
      </Panel>
      <Panel>
        <FlexColumnSpaced>
          {!editor.stale && editor.code.length > 0 && (
            <Button onClick={() => onFormatExpression()}>
              Format
            </Button>
          )}
          <Feedback
            state={state}
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
