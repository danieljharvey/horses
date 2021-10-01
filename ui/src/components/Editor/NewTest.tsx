import * as React from 'react'
import {
  State,
  Action,
  EditorState,
} from '../../reducer/types'
import { CodeEditor } from './CodeEditor'
import { Feedback } from './Feedback'

import { Panel } from '../View/Panel'
import { Button } from '../View/Button'
import {
  getErrorLocations,
  getSourceItems,
  getTypedHoles,
} from '../../reducer/editor/selector'
import { TextInput } from '../View/TextInput'
import { ExprHash } from '../../types'
import { fetchExpressionsForHashes } from '../../reducer/project/actions'
import { flow } from 'fp-ts/function'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'

type Props = {
  state: State
  dispatch: (a: Action) => void
  editor: EditorState
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
}

export const NewTest: React.FC<Props> = ({
  dispatch,
  editor,
  onBindingSelect,
  state,
}) => {
  const [testName, setTestName] = React.useState('')

  const code = editor.code

  const onCodeChange = (a: string) =>
    dispatch({ type: 'UpdateCode', text: a })

  const { expression } = editor

  const testExists =
    editor.expression.type === 'ShowUnitTest'

  const onFetchExpressionsForHashes = flow(
    fetchExpressionsForHashes,
    dispatch
  )

  const onAddTest = () =>
    dispatch({
      type: 'AddUnitTest',
      testName,
    })

  const typedHoleSuggestions = getTypedHoles(state)
  const errorLocations = getErrorLocations(state)

  return (
    <>
      <Panel flexGrow={2}>
        {!testExists && (
          <TextInput
            placeholder="Test name"
            value={testName}
            onChange={setTestName}
          />
        )}
        <CodeEditor
          code={code}
          setCode={onCodeChange}
          sourceItems={getSourceItems(state)}
          errorLocations={errorLocations}
          typedHoleResponses={typedHoleSuggestions}
        />
      </Panel>
      <Panel>
        <FlexColumnSpaced>
          {editor.stale && (
            <Button onClick={onAddTest}>Create test</Button>
          )}
          <Feedback
            result={expression}
            onBindingSelect={onBindingSelect}
            onFetchExpressionsForHashes={
              onFetchExpressionsForHashes
            }
            projectHash={state.project.projectHash}
          />
        </FlexColumnSpaced>
      </Panel>
    </>
  )
}
