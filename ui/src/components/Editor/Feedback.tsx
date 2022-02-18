import * as React from 'react'

import * as O from 'fp-ts/Option'
import { ExpressionResult } from '../../reducer/editor/expressionResult'
import { ListBindings } from '../ListBindings'
import { UnitTest } from '../UnitTest'
import { Code } from '../View/Code'
import { Paragraph } from '../View/Paragraph'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import { BindingVersion, ExprHash } from '../../types'
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

type Props = {
  projectHash: ExprHash
  result: ExpressionResult
  bindingName: O.Option<string>
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
  onUpgradeExpression: (bindingName: string) => void
  onOptimiseExpression: (bindingName: string) => void
}

export const Feedback: React.FC<Props> = ({
  result,
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

  switch (result.type) {
    case 'ShowErrorResponse':
      return (
        <FlexColumnSpaced>
          <Paragraph>
            {result.errorResponse.ueText}
          </Paragraph>
          {result.errorResponse.ueTypedHoles.map((th) => (
            <FlexColumnSpaced>
              <Paragraph>{`${th.thName}: ${th.thMonoType}`}</Paragraph>
              {th.thSuggestions.length > 0 && (
                <Paragraph>
                  {`  Suggestions: ${th.thSuggestions.join(
                    ', '
                  )}`}
                </Paragraph>
              )}
            </FlexColumnSpaced>
          ))}
        </FlexColumnSpaced>
      )

    case 'ShowEvaluate':
      return (
        <FlexColumnSpaced>
          <Code>{result.evaluatedValue}</Code>
          <Code codeType="type">
            {result.expression.edType}
          </Code>
          {result.expression.edWarnings.length > 0 && (
            <FlexColumnSpaced>
              {result.expression.edWarnings.map((warn) => (
                <Paragraph>{warn}</Paragraph>
              ))}
            </FlexColumnSpaced>
          )}
          <ListBindings
            values={result.expression.edBindings}
            types={result.expression.edTypeBindings}
            onBindingSelect={onBindingSelect}
          />
          <ListUsages
            usages={getUsages(result.expression.edHash)}
            onBindingSelect={onBindingSelect}
          />
          <ExpressionTests
            exprHash={result.expression.edHash}
            projectHash={projectHash}
          />
        </FlexColumnSpaced>
      )

    case 'ShowUpdatedBinding':
      return (
        <FlexColumnSpaced>
          <Paragraph>{`🐴 Updated ${result.bindingName}`}</Paragraph>
          <Code codeType="type">
            {result.expression.edType}
          </Code>
          {result.expression.edWarnings.length > 0 && (
            <FlexColumnSpaced>
              {result.expression.edWarnings.map((warn) => (
                <Paragraph>{warn}</Paragraph>
              ))}
            </FlexColumnSpaced>
          )}
          <ListCompile
            exprHash={result.expression.edHash}
          />
          <ListBindings
            values={result.expression.edBindings}
            types={result.expression.edTypeBindings}
            onBindingSelect={onBindingSelect}
          />
          <Upgrade
            onUpgradeExpression={onUpgradeExpression}
            values={result.expression.edBindings}
            types={result.expression.edTypeBindings}
            name={result.bindingName}
            currentHash={result.expression.edHash}
          />
          <Optimise
            onOptimiseExpression={onOptimiseExpression}
            name={result.bindingName}
            canOptimise={result.expression.edCanOptimise}
          />
          <ListVersions
            versions={versions}
            currentHash={result.expression.edHash}
            onBindingSelect={onBindingSelect}
            name={result.bindingName}
          />
          <ListUsages
            usages={getUsages(result.expression.edHash)}
            onBindingSelect={onBindingSelect}
          />
          <ExpressionTests
            exprHash={result.expression.edHash}
            projectHash={projectHash}
          />
        </FlexColumnSpaced>
      )

    case 'ShowBinding':
      return (
        <FlexColumnSpaced>
          <Code codeType="type">
            {result.expression.edType}
          </Code>
          {result.expression.edWarnings.length > 0 && (
            <FlexColumnSpaced>
              {result.expression.edWarnings.map((warn) => (
                <Paragraph>{warn}</Paragraph>
              ))}
            </FlexColumnSpaced>
          )}

          <ListCompile
            exprHash={result.expression.edHash}
          />
          <ListBindings
            values={result.expression.edBindings}
            types={result.expression.edTypeBindings}
            onBindingSelect={onBindingSelect}
          />
          {pipe(
            bindingName,
            O.map((name) => (
              <Upgrade
                onUpgradeExpression={onUpgradeExpression}
                values={result.expression.edBindings}
                types={result.expression.edTypeBindings}
                name={name}
                currentHash={result.expression.edHash}
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
                  result.expression.edCanOptimise
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
                currentHash={result.expression.edHash}
                onBindingSelect={onBindingSelect}
                name={name}
              />
            )),
            O.getOrElse(() => <div />)
          )}

          <ListUsages
            usages={getUsages(result.expression.edHash)}
            onBindingSelect={onBindingSelect}
          />
          <ExpressionTests
            exprHash={result.expression.edHash}
            projectHash={projectHash}
          />
        </FlexColumnSpaced>
      )

    case 'EvaluationError':
      return (
        <FlexColumnSpaced>
          <Paragraph>
            Evaluation error, please try again
          </Paragraph>
        </FlexColumnSpaced>
      )

    case 'ShowTest':
      const title =
        'utdTestName' in result.test
          ? 'Unit test created'
          : 'Property test created'
      return (
        <FlexColumnSpaced>
          <Paragraph>{title}</Paragraph>
          {'utdTestName' in result.test ? (
            <UnitTest unitTest={result.test} />
          ) : (
            <PropertyTest propertyTest={result.test} />
          )}
          <ListBindings
            values={
              'utdBindings' in result.test
                ? result.test.utdBindings
                : result.test.ptdBindings
            }
            types={{}}
            onBindingSelect={onBindingSelect}
          />
        </FlexColumnSpaced>
      )

    case 'EditorNew':
      return <FlexColumnSpaced />
  }
}
