import * as React from 'react'
import { State } from '../../reducer/types'
import { CodeEditor } from './CodeEditor'
import { EditorState } from '../../reducer/editor/types'
import { Feedback } from './Feedback'
import * as O from 'fp-ts/Option'
import { Panel } from '../View/Panel'
import { Button } from '../View/Button'
import { ExprHash } from '../../types'
import {
  updateCode,
  optimiseExpression,
  bindExpression,
  upgradeExpression,
} from '../../reducer/editor/actions'

import {
  getErrorLocations,
  getSourceItems,
  getTypedHoles,
} from '../../reducer/editor/selector'
import { useDispatch } from '../../hooks/useDispatch'

type Props = {
  state: State
  editor: EditorState
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
}

export const EditBinding: React.FC<Props> = ({
  editor,
  onBindingSelect,
  state,
}) => {
  const code = editor.code
  const dispatch = useDispatch()
  const onCodeChange = (a: string) =>
    dispatch(updateCode(a))

  const { expression, stale } = editor

  const bindingName = O.toNullable(editor.bindingName)

  const onBindExpression = () => {
    const bindingName = O.toNullable(editor.bindingName)
    if (bindingName) {
      dispatch(bindExpression(bindingName))
    }
  }

  const onUpgradeExpression = (bindingName: string) =>
    dispatch(upgradeExpression(bindingName))

  const onOptimiseExpression = (bindingName: string) =>
    dispatch(optimiseExpression(bindingName))

  const typedHoleSuggestions = getTypedHoles(state)
  const errorLocations = getErrorLocations(state)

  return (
    <>
      <Panel flexGrow={2}>
        <CodeEditor
          code={code}
          setCode={onCodeChange}
          sourceItems={getSourceItems(state)}
          errorLocations={errorLocations}
          typedHoleResponses={typedHoleSuggestions}
        />
      </Panel>
      <Panel>
        {stale && (
          <Button onClick={() => onBindExpression()}>
            {`Update ${bindingName}`}
          </Button>
        )}
        <Feedback
          bindingName={editor.bindingName}
          state={state}
          result={expression}
          onBindingSelect={onBindingSelect}
          onUpgradeExpression={onUpgradeExpression}
          onOptimiseExpression={onOptimiseExpression}
          projectHash={state.project.projectHash}
        />
      </Panel>
    </>
  )
}
