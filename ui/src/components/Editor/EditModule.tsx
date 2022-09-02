import * as React from 'react'
import { pipe } from 'fp-ts/function'
import { CodeEditor } from './CodeEditor'
import { EditorState } from '../../reducer/editor/types'
import { Feedback } from './Feedback'
import * as O from 'fp-ts/Option'
import { Panel } from '../View/Panel'
import { Button } from '../View/Button'
import { GetModuleResponse, ModuleHash } from '../../types'
import {
  updateCode,
  bindExpression,
} from '../../reducer/editor/actions'
import {
  editorNew,
  showModule,
  Feedback as FeedbackType,
} from '../../reducer/editor/feedback'
import {
  getErrorLocations,
  getSourceItems,
  getTypedHoles,
} from '../../reducer/editor/selector'
import { useDispatch } from '../../hooks/useDispatch'
import { useStoreRec } from '../../hooks/useStore'
import { useModule } from '../../hooks/useModule'

type Props = {
  editor: EditorState
  moduleHash: ModuleHash
  onModuleSelect: (moduleHash: ModuleHash) => void
}

export const EditModule: React.FC<Props> = ({
  editor,
  moduleHash,
}) => {
  const dispatch = useDispatch()

  const [maybeMod] = useModule(moduleHash)

  const code = pipe(
    maybeMod,
    O.fold(
      () => editor.code,
      ({ geModuleData }) => geModuleData.mdModulePretty
    )
  )

  const feedback = pipe(
    maybeMod,
    O.fold<GetModuleResponse, FeedbackType>(
      () => editorNew(),
      ({ geModuleData, geTestData }) =>
        showModule(geModuleData, geTestData)
    )
  )

  const onCodeChange = (a: string) =>
    dispatch(updateCode(a))

  const {
    typedHoleSuggestions,
    errorLocations,
    sourceItems,
  } = useStoreRec({
    typedHoleSuggestions: getTypedHoles,
    errorLocations: getErrorLocations,
    sourceItems: getSourceItems,
  })
  const { stale } = editor

  const bindingName = O.toNullable(editor.bindingName)

  const onBindExpression = () => {
    const bindingName = O.toNullable(editor.bindingName)
    if (bindingName) {
      dispatch(
        bindExpression(bindingName, editor.code, true)
      )
    }
  }

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
        />
      </Panel>
    </>
  )
}
