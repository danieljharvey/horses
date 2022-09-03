import * as React from 'react'

import * as O from 'fp-ts/Option'
import { Feedback as FeedbackType } from '../../reducer/editor/feedback'
import { ListBindings } from '../ListBindings'
import { Code } from '../View/Code'
import { Paragraph } from '../View/Paragraph'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import { ListTests } from '../ListTests'
import {
  BindingVersion,
  ExprHash,
  exprHash,
} from '../../types'
import { ListVersions } from '../ListVersions'
import {
  getUsagesOfExprHash,
  getVersionsOfBinding,
} from '../../reducer/project/selectors'
import { pipe } from 'fp-ts/function'
import { ListCompile } from '../ListCompile'
import { ListUsages } from '../ListUsages'
import { useStoreRec } from '../../hooks/useStore'
import { Expression } from './Expression'
import { ErrorResponse } from './ErrorResponse'

type Props = {
  feedback: FeedbackType
  bindingName: O.Option<string>
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
}

export const Feedback: React.FC<Props> = ({
  feedback,
  bindingName,
  onBindingSelect,
}) => {
  const { getVersions, getUsages } = useStoreRec({
    getVersions: getVersionsOfBinding,
    getUsages: getUsagesOfExprHash,
  })
  // need to return new bindings and typeBindings
  const versions = pipe(
    bindingName,
    O.map((name) => getVersions(name)),
    O.getOrElse(() => [] as BindingVersion[])
  )

  switch (feedback.type) {
    case 'ShowErrorResponse':
      return (
        <ErrorResponse
          errorResponse={feedback.errorResponse}
        />
      )

    case 'ShowEvaluate':
      return (
        <FlexColumnSpaced>
          <Code>{feedback.evaluatedValue}</Code>
          <Expression expression={feedback.expression} />
          <ListBindings
            modules={{}}
            onModuleSelect={() => {}}
            values={
              feedback.expression.edBindings as Record<
                string,
                ExprHash
              >
            }
            types={
              feedback.expression.edTypeBindings as Record<
                string,
                ExprHash
              >
            }
            onBindingSelect={onBindingSelect}
          />
          <ListUsages
            usages={getUsages(
              exprHash(feedback.expression.edHash)
            )}
            onBindingSelect={onBindingSelect}
          />
        </FlexColumnSpaced>
      )

    case 'ShowUpdatedBinding':
      return (
        <FlexColumnSpaced>
          <Paragraph>{`🐴 Updated ${feedback.bindingName}`}</Paragraph>
          <Expression expression={feedback.expression} />
          <ListCompile
            exprHash={exprHash(feedback.expression.edHash)}
          />
          <ListBindings
            modules={{}}
            onModuleSelect={() => {}}
            values={
              feedback.expression.edBindings as Record<
                string,
                ExprHash
              >
            }
            types={
              feedback.expression.edTypeBindings as Record<
                string,
                ExprHash
              >
            }
            onBindingSelect={onBindingSelect}
          />
          <ListVersions
            versions={versions}
            currentHash={exprHash(
              feedback.expression.edHash
            )}
            onBindingSelect={onBindingSelect}
            name={feedback.bindingName}
          />
          <ListUsages
            usages={getUsages(
              exprHash(feedback.expression.edHash)
            )}
            onBindingSelect={onBindingSelect}
          />
        </FlexColumnSpaced>
      )

    case 'ShowModuleData':
      return (
        <FlexColumnSpaced>
          <Code codeType="type">
            {feedback.moduleData.mdModuleType}
          </Code>
          <ListTests
            propertyTests={
              feedback.testData.tdPropertyTests
            }
            unitTests={feedback.testData.tdUnitTests}
          />
          <ListBindings
            modules={{}}
            onModuleSelect={() => {}}
            values={{}}
            types={{}}
            onBindingSelect={onBindingSelect}
          />
          <ListUsages
            usages={[]}
            onBindingSelect={onBindingSelect}
          />
        </FlexColumnSpaced>
      )

    case 'ShowBinding':
      return (
        <FlexColumnSpaced>
          <Expression expression={feedback.expression} />
          <ListCompile
            exprHash={exprHash(feedback.expression.edHash)}
          />
          <ListBindings
            modules={{}}
            onModuleSelect={() => {}}
            values={
              feedback.expression.edBindings as Record<
                string,
                ExprHash
              >
            }
            types={
              feedback.expression.edTypeBindings as Record<
                string,
                ExprHash
              >
            }
            onBindingSelect={onBindingSelect}
          />
          {pipe(
            bindingName,
            O.map((name) => (
              <ListVersions
                versions={versions}
                currentHash={exprHash(
                  feedback.expression.edHash
                )}
                onBindingSelect={onBindingSelect}
                name={name}
              />
            )),
            O.getOrElse(() => <div />)
          )}
          <ListUsages
            usages={getUsages(
              exprHash(feedback.expression.edHash)
            )}
            onBindingSelect={onBindingSelect}
          />
        </FlexColumnSpaced>
      )

    case 'EditorNew':
      return <FlexColumnSpaced />

    case 'ShowPreviewSuccess':
      return (
        <FlexColumnSpaced>
          <Expression expression={feedback.expression} />
        </FlexColumnSpaced>
      )
  }
}
