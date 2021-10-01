import * as React from 'react'
import * as O from 'fp-ts/Option'
import * as E from 'fp-ts/Either'
import {
  State,
  Action,
  EditorState,
} from '../../reducer/types'
import { pipe, flow } from 'fp-ts/function'
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
import { fetchExpressionsForHashes } from '../../reducer/project/actions'
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

export const NewBinding: React.FC<Props> = ({
  state,
  dispatch,
  editor,
  onBindingSelect,
}) => {
  const [bindingName, setBindingName] = React.useState('')

  const code = editor.code

  const onCodeChange = (a: string) =>
    dispatch({ type: 'UpdateCode', text: a })

  const { expression } = editor

  const existingName = O.toNullable(editor.bindingName)

  const onBindExpression = (name: string) =>
    dispatch({
      type: 'BindExpression',
      bindingName: existingName || name,
    })

  const onFetchExpressionsForHashes = flow(
    fetchExpressionsForHashes,
    dispatch
  )
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
                  result={expression}
                  onBindingSelect={onBindingSelect}
                  onFetchExpressionsForHashes={
                    onFetchExpressionsForHashes
                  }
                  projectHash={state.project.projectHash}
                />
              </FlexColumnSpaced>
            )
          )
        )}
      </Panel>
    </>
  )
}
