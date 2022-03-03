import * as React from 'react'
import * as O from 'fp-ts/Option'
import * as E from 'fp-ts/Either'
import { EditorState } from '../../reducer/editor/types'
import { pipe } from 'fp-ts/function'
import { CodeEditor } from './CodeEditor'
import { Feedback } from './Feedback'
import {
  getProjectBindings,
  getProjectHash,
} from '../../reducer/project/selectors'
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

import { TextInput } from '../View/TextInput'
import { ExprHash } from '../../types'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import {
  bindExpression,
  upgradeExpression,
  optimiseExpression,
} from '../../reducer/editor/actions'
import { useDispatch } from '../../hooks/useDispatch'
import { useStoreRec } from '../../hooks/useStore'

import { Paragraph } from '../View/Paragraph'

type Props = {
  editor: EditorState
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
}

export const NewBinding: React.FC<Props> = ({
  editor,
  onBindingSelect,
}) => {
  const [bindingName, setBindingName] = React.useState('')
  const dispatch = useDispatch()

  const { feedback, code } = editor

  // on opening, evaluate contents
  React.useEffect(() => {
    onCodeChange(code)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  const onCodeChange = (newCode: string) =>
    dispatch(
      bindExpression(
        'completeArbitraryNameForBindingThatHopefullyDoesntClash',
        newCode,
        false
      )
    )

  const {
    typedHoleSuggestions,
    errorLocations,
    sourceItems,
    projectHash,
    projectBindings,
  } = useStoreRec({
    typedHoleSuggestions: getTypedHoles,
    errorLocations: getErrorLocations,
    sourceItems: getSourceItems,
    projectHash: getProjectHash,
    projectBindings: getProjectBindings,
  })

  const onBindExpression = (name: string) =>
    dispatch(bindExpression(name, code, true))

  const onUpgradeExpression = (bindingName: string) =>
    dispatch(upgradeExpression(bindingName))

  const onOptimiseExpression = (bindingName: string) =>
    dispatch(optimiseExpression(bindingName))

  return (
    <>
      <Panel flexGrow={2}>
        <FlexColumnSpaced>
          <TextInput
            placeholder="New binding name"
            value={bindingName}
            onChange={setBindingName}
          />
          <CodeEditor
            code={code}
            setCode={onCodeChange}
            sourceItems={sourceItems}
            errorLocations={errorLocations}
            typedHoleResponses={typedHoleSuggestions}
          />
        </FlexColumnSpaced>
      </Panel>
      <Panel>
        <FlexColumnSpaced>
          {pipe(
            validateBinding(
              bindingName,
              code,
              projectBindings
            ),
            E.match(
              (e) => <Paragraph>{showError(e)}</Paragraph>,
              (name) =>
                editor.stale && (
                  <Button
                    onClick={() => onBindExpression(name)}
                  >
                    {`Create ${name}`}
                  </Button>
                )
            )
          )}
          <Feedback
            bindingName={O.none}
            feedback={feedback}
            onBindingSelect={onBindingSelect}
            projectHash={projectHash}
            onUpgradeExpression={onUpgradeExpression}
            onOptimiseExpression={onOptimiseExpression}
          />
        </FlexColumnSpaced>
      </Panel>
    </>
  )
}
