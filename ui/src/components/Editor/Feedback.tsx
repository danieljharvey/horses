import * as React from 'react'

import * as O from 'fp-ts/Option'
import { Feedback as FeedbackType } from '../../reducer/editor/feedback'
import { ListBindings } from '../ListBindings'
import { UnitTest } from '../UnitTest'
import { Code } from '../View/Code'
import { Paragraph } from '../View/Paragraph'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import {
  BindingVersion,
  ProjectHash,
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
import { PropertyTest } from '../PropertyTest'
import { ExpressionTests } from './ExpressionTests'
import { Upgrade } from '../Upgrade'
import { Optimise } from '../Optimise'
import { useStoreRec } from '../../hooks/useStore'
import { Expression } from './Expression'
import { ErrorResponse } from './ErrorResponse'

type Props = {
  projectHash: ProjectHash
  feedback: FeedbackType
  bindingName: O.Option<string>
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
  onUpgradeExpression: (bindingName: string) => void
  onOptimiseExpression: (bindingName: string) => void
}

export const Feedback: React.FC<Props> = ({
  feedback,
  bindingName,
  projectHash,
  onBindingSelect,
  onUpgradeExpression,
  onOptimiseExpression,
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
          <ExpressionTests
            exprHash={exprHash(feedback.expression.edHash)}
            projectHash={projectHash}
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
          <Upgrade
            onUpgradeExpression={onUpgradeExpression}
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
            name={feedback.bindingName}
            currentHash={exprHash(
              feedback.expression.edHash
            )}
          />
          <Optimise
            onOptimiseExpression={onOptimiseExpression}
            name={feedback.bindingName}
            canOptimise={feedback.expression.edCanOptimise}
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
          <ExpressionTests
            exprHash={exprHash(feedback.expression.edHash)}
            projectHash={projectHash}
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
              <Upgrade
                onUpgradeExpression={onUpgradeExpression}
                values={
                  feedback.expression.edBindings as Record<
                    string,
                    ExprHash
                  >
                }
                types={
                  feedback.expression
                    .edTypeBindings as Record<
                    string,
                    ExprHash
                  >
                }
                name={name}
                currentHash={exprHash(
                  feedback.expression.edHash
                )}
              />
            )),
            O.getOrElse(() => <div />)
          )}
          {pipe(
            bindingName,
            O.map((name) => (
              <Optimise
                onOptimiseExpression={onOptimiseExpression}
                name={name}
                canOptimise={
                  feedback.expression.edCanOptimise
                }
              />
            )),
            O.getOrElse(() => <div />)
          )}
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
          <ExpressionTests
            exprHash={exprHash(feedback.expression.edHash)}
            projectHash={projectHash}
          />
        </FlexColumnSpaced>
      )

    case 'ShowTest':
      const title =
        'utdTestName' in feedback.test
          ? 'Unit test created'
          : 'Property test created'
      return (
        <FlexColumnSpaced>
          <Paragraph>{title}</Paragraph>
          {'utdTestName' in feedback.test ? (
            <UnitTest unitTest={feedback.test} />
          ) : (
            <PropertyTest propertyTest={feedback.test} />
          )}
          <ListBindings
            values={
              ('utdBindings' in feedback.test
                ? feedback.test.utdBindings
                : feedback.test.ptdBindings) as Record<
                string,
                ExprHash
              >
            }
            types={{}}
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
