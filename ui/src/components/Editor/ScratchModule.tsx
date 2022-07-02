import * as React from 'react'
import { EditorState } from '../../reducer/editor/types'
import { CodeEditor } from './CodeEditor'
import { Feedback } from './Feedback'
import { Panel } from '../View/Panel'
import * as O from 'fp-ts/Option'
import { Button } from '../View/Button'
import { ExprHash, ProjectHash } from '../../types'
import { useDispatch } from '../../hooks/useDispatch'
import { updateCode } from '../../reducer/editor/actions'

import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import { useEvaluateExpression } from '../../hooks/useEvaluateExpression'

type Props = {
  projectHash: ProjectHash
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
  editor: EditorState
}

export const ScratchModule: React.FC<Props> = ({
  onBindingSelect,
  projectHash,
  editor,
}) => {
  const [code, setCode] = React.useState(editor.code)
  const [feedback, stale] = useEvaluateExpression(
    projectHash,
    code
  )

  const formattedVersion =
    feedback.type === 'ShowEvaluate'
      ? feedback.expression.edPretty
      : null

  const onFormatExpression = () => {
    if (formattedVersion) {
      setCode(formattedVersion)
    }
  }

  const dispatch = useDispatch()

  // we store both locally (to use the hook)
  // and globally (so it's saved when we navigate back to this page)
  const onCodeChange = (newCode: string) => {
    dispatch(updateCode(newCode))
    setCode(newCode)
  }

  const onUpgradeExpression = (_bindingName: string) => {}

  const onOptimiseExpression = (_bindingName: string) => {}

  const typedHoleResponses =
    feedback.type === 'ShowErrorResponse'
      ? feedback.errorResponse.ueTypedHoles
      : []

  const errorLocations =
    feedback.type === 'ShowErrorResponse'
      ? feedback.errorResponse.ueErrorLocations
      : []

  const sourceItems =
    feedback.type === 'ShowEvaluate'
      ? feedback.expression.edSourceItems
      : []

  return (
    <>
      <Panel flexGrow={2}>
        <CodeEditor
          code={code}
          sourceItems={sourceItems}
          setCode={onCodeChange}
          errorLocations={errorLocations}
          typedHoleResponses={typedHoleResponses}
        />
      </Panel>
      <Panel>
        <FlexColumnSpaced>
          {!stale &&
            code.length > 0 &&
            (formattedVersion || '') !== code && (
              <Button onClick={() => onFormatExpression()}>
                Format
              </Button>
            )}
          <Feedback
            bindingName={O.none}
            onUpgradeExpression={onUpgradeExpression}
            feedback={feedback}
            onBindingSelect={onBindingSelect}
            projectHash={projectHash}
            onOptimiseExpression={onOptimiseExpression}
          />
        </FlexColumnSpaced>
      </Panel>
    </>
  )
}
