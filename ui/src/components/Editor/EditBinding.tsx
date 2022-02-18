import * as React from 'react'
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
import { useStoreRec } from '../../hooks/useStore'
import { getProjectHash } from '../../reducer/project/selectors'

type Props = {
  editor: EditorState
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
}

export const EditBinding: React.FC<Props> = ({
  editor,
  onBindingSelect,
}) => {
  const code = editor.code
  const dispatch = useDispatch()

  const onCodeChange = (a: string) =>
    dispatch(updateCode(a))

  const {
    typedHoleSuggestions,
    errorLocations,
    sourceItems,
    projectHash,
  } = useStoreRec({
    typedHoleSuggestions: getTypedHoles,
    errorLocations: getErrorLocations,
    sourceItems: getSourceItems,
    projectHash: getProjectHash,
  })
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

  return (
    <>
      <Panel flexGrow={2}>
        <CodeEditor
          code={code}
          setCode={onCodeChange}
          sourceItems={sourceItems}
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
          result={expression}
          onBindingSelect={onBindingSelect}
          onUpgradeExpression={onUpgradeExpression}
          onOptimiseExpression={onOptimiseExpression}
          projectHash={projectHash}
        />
      </Panel>
    </>
  )
}
