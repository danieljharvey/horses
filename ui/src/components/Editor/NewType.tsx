import * as React from 'react'
import {
  State,
  Action,
  EditorState,
} from '../../reducer/types'
import { storeProjectData } from '../../reducer/project/reducer'
import { getSourceItems } from '../../reducer/editor/selector'
import { pipe } from 'fp-ts/function'
import { CodeEditor } from './CodeEditor'
import { Feedback } from './Feedback'
import { Code } from '../View/Code'
import { Panel } from '../View/Panel'
import { useAddType } from '../../hooks/useAddType'
import { fold } from '@devexperts/remote-data-ts'
import { Link } from '../View/Link'
import { Button } from '../View/Button'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import { Paragraph } from '../View/Paragraph'
import { ListBindings } from '../ListBindings'
import { InlineSpaced } from '../View/InlineSpaced'
import { ExprHash, UserErrorResponse } from '../../types'
import * as O from 'fp-ts/Option'

type Props = {
  state: State
  dispatch: (a: Action) => void
  editor: EditorState
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
}

export const NewType: React.FC<Props> = ({
  state,
  dispatch,
  editor,
  onBindingSelect,
}) => {
  const projectHash = state.project.projectHash

  const [addNewType, typeState] = useAddType(
    projectHash,
    editor.code,
    (pd) => dispatch(storeProjectData(pd))
  )

  const onCodeChange = (a: string) =>
    dispatch({ type: 'UpdateCode', text: a })

  const { expression, code } = editor

  return (
    <>
      {pipe(
        typeState,
        fold(
          () => (
            <>
              <Panel flexGrow={2}>
                <CodeEditor
                  code={code}
                  setCode={onCodeChange}
                  sourceItems={getSourceItems(state)}
                  errorLocations={[]}
                  typedHoleResponses={[]}
                />
              </Panel>
              <Panel>
                <FlexColumnSpaced>
                  <Feedback
                    bindingName={O.none}
                    state={state}
                    result={expression}
                    onBindingSelect={onBindingSelect}
                    projectHash={projectHash}
                  />
                  {editor.stale && (
                    <Button onClick={addNewType}>
                      Create
                    </Button>
                  )}
                </FlexColumnSpaced>
              </Panel>
            </>
          ),
          () => <p>Loading</p>,

          (err: UserErrorResponse) => (
            <>
              <Panel flexGrow={2}>
                <CodeEditor
                  code={code}
                  setCode={onCodeChange}
                  sourceItems={getSourceItems(state)}
                  errorLocations={err.ueErrorLocations}
                  typedHoleResponses={err.ueTypedHoles}
                />
              </Panel>
              <Panel>
                {editor.stale && (
                  <Button onClick={addNewType}>
                    Create
                  </Button>
                )}
                <Code>{err.ueText}</Code>
              </Panel>
            </>
          ),
          (addType) => (
            <Panel>
              <FlexColumnSpaced>
                <Paragraph>{`New type added: ${addType.typeName}`}</Paragraph>
                <Code>{addType.dataTypePretty}</Code>
                <Paragraph>Typeclasses:</Paragraph>
                <InlineSpaced>
                  {addType.typeclasses.map((a) => (
                    <Link
                      number={1}
                      depType="type"
                      onClick={() => {}}
                      highlight={true}
                    >
                      {a}
                    </Link>
                  ))}
                </InlineSpaced>
                <Paragraph>Generated functions:</Paragraph>
                <ListBindings
                  state={state}
                  values={addType.bindings}
                  types={addType.typeBindings}
                  onBindingSelect={onBindingSelect}
                />
              </FlexColumnSpaced>
            </Panel>
          )
        )
      )}
    </>
  )
}
