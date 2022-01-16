import * as React from 'react'
import {
  Action,
  EditorState,
  State,
} from '../../reducer/types'
import { CodeEditor } from './CodeEditor'
import { Feedback } from './Feedback'
import { Panel } from '../View/Panel'
import * as O from 'fp-ts/Option'
import { Button } from '../View/Button'
import { ExprHash } from '../../types'

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

  const onUpgradeExpression = (bindingName: string) =>
    dispatch({ type: 'UpgradeExpression', bindingName })

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
            bindingName={O.none}
            onUpgradeExpression={onUpgradeExpression}
            state={state}
            result={editor.expression}
            onBindingSelect={onBindingSelect}
            projectHash={projectHash}
          />
        </FlexColumnSpaced>
      </Panel>
    </>
  )
}
