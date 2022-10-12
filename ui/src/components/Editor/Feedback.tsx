import * as React from 'react'

import * as O from 'fp-ts/Option'
import { Feedback as FeedbackType } from '../../reducer/editor/feedback'
import { ListBindings } from '../ListBindings'
import { Code } from '../View/Code'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import { ListTests } from '../ListTests'
import { exprHash } from '../../types'
import { ListCompile } from '../ListCompile'
import { Expression } from './Expression'
import { ErrorResponse } from './ErrorResponse'

type Props = {
  feedback: FeedbackType
  bindingName: O.Option<string>
}

export const Feedback: React.FC<Props> = ({ feedback }) => {
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
          />
        </FlexColumnSpaced>
      )

    case 'EditorNew':
      return <FlexColumnSpaced />
  }
}
