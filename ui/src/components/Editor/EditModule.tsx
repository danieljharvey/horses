import * as React from 'react'
import { CodeEditor } from './CodeEditor'
import { EditorState } from '../../reducer/editor/types'
import { Feedback } from './Feedback'
import * as O from 'fp-ts/Option'
import { Panel } from '../View/Panel'
import { Button } from '../View/Button'
import { ModuleHash } from '../../types'
import {
  updateCode,
  bindExpression,
} from '../../reducer/editor/actions'
import { fetchModule } from '../../reducer/project/actions'
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
  moduleHash:ModuleHash
  onModuleSelect: (
    moduleHash: ModuleHash
  ) => void
}

export const EditModule: React.FC<Props> = ({
  editor,
  moduleHash,
}) => {
  const code = editor.code
  const dispatch = useDispatch()
  
  React.useEffect(() => {
    dispatch(fetchModule(moduleHash)) 
  },[moduleHash])
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
  const { feedback, stale } = editor

  const bindingName = O.toNullable(editor.bindingName)

  const onBindExpression = () => {
    const bindingName = O.toNullable(editor.bindingName)
    if (bindingName) {
      dispatch(
        bindExpression(bindingName, editor.code, true)
      )
    }
  }

  const onUpgradeExpression = (_bindingName: string) => {}

  const onOptimiseExpression = (_bindingName: string) => {}

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
          feedback={feedback}
          onBindingSelect={() => {}}
          onUpgradeExpression={onUpgradeExpression}
          onOptimiseExpression={onOptimiseExpression}
          projectHash={projectHash}
        />
      </Panel>
    </>
  )
}
