import * as React from 'react'
import { EditorState } from '../../reducer/editor/types'
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
import {
  formatExpression,
  evaluateExpression,
} from '../../reducer/editor/actions'
import { useDispatch } from '../../hooks/useDispatch'

type Props = {
  editor: EditorState
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
}

export const Scratch: React.FC<Props> = ({
  editor,
  onBindingSelect,
}) => {
  const dispatch = useDispatch()

  const onFormatExpression = () =>
    dispatch(formatExpression())

  const onCodeChange = (text: string) =>
    dispatch(evaluateExpression(text))

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
            feedback={editor.feedback}
            onBindingSelect={onBindingSelect}
          />
        </FlexColumnSpaced>
      </Panel>
    </>
  )
}
