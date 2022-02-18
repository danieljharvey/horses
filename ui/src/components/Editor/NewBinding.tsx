import * as React from 'react'
import * as O from 'fp-ts/Option'
import * as E from 'fp-ts/Either'
import { State } from '../../reducer/types'
import { EditorState } from '../../reducer/editor/types'
import { pipe } from 'fp-ts/function'
import { CodeEditor } from './CodeEditor'
import { Feedback } from './Feedback'
import { getProjectBindings } from '../../reducer/project/selectors'
import {
  validateBinding,
  showError,
} from './utils/validateBindingName'
import {
  getErrorLocations,
  getSourceItems,
  getTypedHoles,
} from '../../reducer/editor/selector'

import { Panel } from '../View/Panel'
import { Button } from '../View/Button'
import { Paragraph } from '../View/Paragraph'
import { TextInput } from '../View/TextInput'
import { ExprHash } from '../../types'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import {
  bindExpression,
  updateCode,
  upgradeExpression,
  optimiseExpression,
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

export const NewBinding: React.FC<Props> = ({
  state,
  editor,
  onBindingSelect,
}) => {
  const [bindingName, setBindingName] = React.useState('')
  const dispatch = useDispatch()
  const code = editor.code

  const onCodeChange = (a: string) =>
    dispatch(updateCode(a))

  const { expression } = editor

  const existingName = O.toNullable(editor.bindingName)

  const onBindExpression = (name: string) =>
    dispatch(bindExpression(existingName || name))

  const onUpgradeExpression = (bindingName: string) =>
    dispatch(upgradeExpression(bindingName))

  const onOptimiseExpression = (bindingName: string) =>
    dispatch(optimiseExpression(bindingName))

  const validBinding = existingName
    ? E.right(existingName)
    : validateBinding(
        bindingName,
        code,
        getProjectBindings(state)
      )

  const typedHoleSuggestions = getTypedHoles(state)
  const errorLocations = getErrorLocations(state)

  return (
    <>
      <Panel flexGrow={2}>
        <FlexColumnSpaced>
          {!existingName && (
            <TextInput
              placeholder="New binding name"
              value={bindingName}
              onChange={setBindingName}
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
        {pipe(
          validBinding,
          E.fold(
            (err) => (
              <Paragraph>{showError(err)}</Paragraph>
            ),
            (name) => (
              <FlexColumnSpaced>
                {editor.stale && (
                  <Button
                    onClick={() => onBindExpression(name)}
                  >
                    {existingName
                      ? `Update ${existingName}`
                      : `Create ${name}`}
                  </Button>
                )}
                <Feedback
                  bindingName={O.none}
                  state={state}
                  result={expression}
                  onBindingSelect={onBindingSelect}
                  projectHash={state.project.projectHash}
                  onUpgradeExpression={onUpgradeExpression}
                  onOptimiseExpression={
                    onOptimiseExpression
                  }
                />
              </FlexColumnSpaced>
            )
          )
        )}
      </Panel>
    </>
  )
}
