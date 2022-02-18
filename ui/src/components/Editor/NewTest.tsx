import * as React from 'react'
import { State } from '../../reducer/types'
import { EditorState } from '../../reducer/editor/types'
import { CodeEditor } from './CodeEditor'
import { Feedback } from './Feedback'
import * as O from 'fp-ts/Option'

import { Panel } from '../View/Panel'
import { Button } from '../View/Button'
import {
  getErrorLocations,
  getSourceItems,
  getTypedHoles,
} from '../../reducer/editor/selector'
import { TextInput } from '../View/TextInput'
import { ExprHash } from '../../types'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import {
  addUnitTest,
  updateCode,
  optimiseExpression,
  upgradeExpression,
} from '../../reducer/editor/actions'
import { useDispatch } from '../../hooks/useDispatch'

type Props = {
  state: State
  editor: EditorState
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
}

export const NewTest: React.FC<Props> = ({
  editor,
  onBindingSelect,
  state,
}) => {
  const [testName, setTestName] = React.useState('')
  const dispatch = useDispatch()

  const code = editor.code

  const onCodeChange = (a: string) =>
    dispatch(updateCode(a))

  const { expression } = editor

  const testExists = editor.expression.type === 'ShowTest'

  const onAddTest = () => dispatch(addUnitTest(testName))

  const onUpgradeExpression = (bindingName: string) =>
    dispatch(upgradeExpression(bindingName))

  const onOptimiseExpression = (bindingName: string) =>
    dispatch(optimiseExpression(bindingName))

  const typedHoleSuggestions = getTypedHoles(state)
  const errorLocations = getErrorLocations(state)

  return (
    <>
      <Panel flexGrow={2}>
        <FlexColumnSpaced>
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
        </FlexColumnSpaced>
      </Panel>
      <Panel>
        <FlexColumnSpaced>
          {editor.stale && (
            <Button onClick={onAddTest}>Create test</Button>
          )}
          <Feedback
            bindingName={O.none}
            state={state}
            result={expression}
            onBindingSelect={onBindingSelect}
            onUpgradeExpression={onUpgradeExpression}
            projectHash={state.project.projectHash}
            onOptimiseExpression={onOptimiseExpression}
          />
        </FlexColumnSpaced>
      </Panel>
    </>
  )
}
