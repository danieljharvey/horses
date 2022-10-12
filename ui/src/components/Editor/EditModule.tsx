import * as React from 'react'
import { CodeEditor } from './CodeEditor'
import { pipe } from 'fp-ts/function'
import * as O from 'fp-ts/Option'
import { Feedback } from './Feedback'
import { Panel } from '../View/Panel'
import {
  ModuleData,
  UserErrorResponse,
  ModuleHash,
} from '../../types'
import {
  editorNew,
  showErrorResponse,
  showModule,
  Feedback as FeedbackType,
} from '../../reducer/editor/feedback'
import {
  getErrorLocations,
  getSourceItems,
  getTypedHoles,
} from '../../reducer/editor/selector'
import { useStoreRec } from '../../hooks/useStore'
import {
  BindModule,
  useBindModule,
} from '../../hooks/useBindModule'
import { getProjectHash } from '../../reducer/project/selectors'
import { fold } from '@devexperts/remote-data-ts'

type Props = {
  moduleData: ModuleData
  onModuleSelect: (moduleHash: ModuleHash) => void
}

export const EditModule: React.FC<Props> = ({
  moduleData,
}) => {
  const [userInput, setUserInput] = React.useState<string>(
    moduleData.mdModulePretty
  )

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

  const [bindModule, boundModule] = useBindModule(
    projectHash,
    userInput,
    undefined,
    () => {}
  )

  const onCodeChange = (str: string) => {
    setUserInput(str)
    bindModule()
  }

  // this would be nicer if we kept the old feedback and only updated when we
  // found new useful info, otherwise it flashes lots
  const feedback = pipe(
    boundModule,
    fold<UserErrorResponse, BindModule, FeedbackType>(
      () => editorNew(),
      () => editorNew(), // loading?
      (e) => showErrorResponse(e),
      ({ moduleData, testData }) =>
        showModule(moduleData, testData)
    )
  )

  /*O.fold<GetModuleResponse, FeedbackType>(
      () => editorNew(),
      ({ geModuleData, geTestData }) =>
        showModule(geModuleData, geTestData)
    )*/

  return (
    <>
      <Panel flexGrow={2}>
        <CodeEditor
          code={userInput}
          setCode={onCodeChange}
          sourceItems={sourceItems}
          errorLocations={errorLocations}
          typedHoleResponses={typedHoleSuggestions}
        />
      </Panel>
      <Panel>
        <Feedback
          bindingName={O.none}
          feedback={feedback}
        />
      </Panel>
    </>
  )
}
