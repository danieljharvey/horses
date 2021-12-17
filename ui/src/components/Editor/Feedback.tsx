import * as React from 'react'

import * as O from 'fp-ts/Option'
import { ExpressionResult } from '../../reducer/editor/reducer'
import { ListBindings } from '../ListBindings'
import { UnitTest } from '../UnitTest'
import { ListTests } from '../ListTests'
import { Code } from '../View/Code'
import { Paragraph } from '../View/Paragraph'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import { BindingVersion, ExprHash } from '../../types'
import { State } from '../../reducer/types'
import { ListVersions } from '../ListVersions'
import {
  getUsagesOfExprHash,
  getVersionsOfBinding,
} from '../../reducer/project/selectors'
import { pipe } from 'fp-ts/function'
import { ListCompile } from '../ListCompile'
import { ListUsages } from '../ListUsages'
import { PropertyTest } from '../PropertyTest'

type Props = {
  projectHash: ExprHash
  result: ExpressionResult
  bindingName: O.Option<string>
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
  state: State
}

export const Feedback: React.FC<Props> = ({
  result,
  bindingName,
  state,
  onBindingSelect,
}) => {
  // need to return new bindings and typeBindings
  const versions = pipe(
    bindingName,
    O.map((name) => getVersionsOfBinding(name, state)),
    O.getOrElse(() => [] as BindingVersion[])
  )

  const getUsages = (exprHash: ExprHash) =>
    getUsagesOfExprHash(exprHash, state)
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
          <ListBindings
            state={state}
            values={result.expression.edBindings}
            types={result.expression.edTypeBindings}
            onBindingSelect={onBindingSelect}
          />
          <ListUsages
            usages={getUsages(result.expression.edHash)}
            onBindingSelect={onBindingSelect}
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
          <ListTests
            unitTests={result.tests.tdUnitTests}
            propertyTests={result.tests.tdPropertyTests}
          />
          <ListCompile
            runtimes={Object.values(
              result.expression.edRuntimes
            )}
            exprHash={result.expression.edHash}
          />
          <ListBindings
            state={state}
            values={result.expression.edBindings}
            types={result.expression.edTypeBindings}
            onBindingSelect={onBindingSelect}
          />
          {pipe(
            bindingName,
            O.map((name) => (
              <ListVersions
                versions={versions}
                currentHash={result.expression.edHash}
                onBindingSelect={onBindingSelect}
                name={name}
                state={state}
              />
            )),
            O.getOrElse(() => <div />)
          )}
          <ListUsages
            usages={getUsages(result.expression.edHash)}
            onBindingSelect={onBindingSelect}
          />
        </FlexColumnSpaced>
      )

    case 'ShowBinding':
      return (
        <FlexColumnSpaced>
          <Code codeType="type">
            {result.expression.edType}
          </Code>
          <ListTests
            unitTests={result.tests.tdUnitTests}
            propertyTests={result.tests.tdPropertyTests}
          />
          <ListCompile
            runtimes={Object.values(
              result.expression.edRuntimes
            )}
            exprHash={result.expression.edHash}
          />
          <ListBindings
            state={state}
            values={result.expression.edBindings}
            types={result.expression.edTypeBindings}
            onBindingSelect={onBindingSelect}
          />
          {pipe(
            bindingName,
            O.map((name) => (
              <ListVersions
                versions={versions}
                currentHash={result.expression.edHash}
                onBindingSelect={onBindingSelect}
                name={name}
                state={state}
              />
            )),
            O.getOrElse(() => <div />)
          )}
          <ListUsages
            usages={getUsages(result.expression.edHash)}
            onBindingSelect={onBindingSelect}
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
            state={state}
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
