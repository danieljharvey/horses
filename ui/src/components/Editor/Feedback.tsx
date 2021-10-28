import * as React from 'react'

import { ExpressionResult } from '../../reducer/editor/reducer'
import { ListBindings } from '../ListBindings'
import { UnitTest } from '../UnitTest'
import { ListTests } from '../ListTests'
import { Code } from '../View/Code'
import { Paragraph } from '../View/Paragraph'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import { ExprHash } from '../../types'
import { Compile } from './Compile'

type Props = {
  projectHash: ExprHash
  result: ExpressionResult
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
  onFetchExpressionsForHashes: (hashes: ExprHash[]) => void
}

export const Feedback: React.FC<Props> = ({
  result,
  onBindingSelect,
  onFetchExpressionsForHashes,
}) => {
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
          {/*<Paragraph>#{result.expression.edHash}</Paragraph>*/}
          <Code>{result.evaluatedValue}</Code>
          <Code codeType="type">
            {result.expression.edType}
          </Code>
          <ListTests
            unitTests={result.expression.edUnitTests}
          />
          <ListBindings
            values={result.expression.edBindings}
            types={result.expression.edTypeBindings}
            onBindingSelect={onBindingSelect}
            onFetchExpressionsForHashes={
              onFetchExpressionsForHashes
            }
          />
        </FlexColumnSpaced>
      )
    case 'ShowUpdatedBinding':
      // need to return new bindings and typeBindings
      return (
        <FlexColumnSpaced>
          <Paragraph>{`🐴 Updated ${result.bindingName}`}</Paragraph>
          <Code codeType="type">
            {result.expression.edType}
          </Code>
          <ListTests
            unitTests={result.expression.edUnitTests}
          />
          {Object.values(result.expression.edRuntimes).map(
            (rt) => (
              <>
                <Compile
                  exprHash={result.expression.edHash}
                  runtime={rt.rtdName}
                  title={rt.rtdName}
                />
              </>
            )
          )}
          <ListBindings
            values={result.expression.edBindings}
            types={result.expression.edTypeBindings}
            onBindingSelect={onBindingSelect}
            onFetchExpressionsForHashes={
              onFetchExpressionsForHashes
            }
          />
        </FlexColumnSpaced>
      )

    case 'ShowBinding':
      return (
        <FlexColumnSpaced>
          {/*<Paragraph>
            🐴 {result.expression.edHash}
          </Paragraph>*/}
          <Code codeType="type">
            {result.expression.edType}
          </Code>
          <ListTests
            unitTests={result.expression.edUnitTests}
          />
          {Object.values(result.expression.edRuntimes).map(
            (rt) => (
              <FlexColumnSpaced>
                <Compile
                  exprHash={result.expression.edHash}
                  runtime={rt.rtdName}
                  title={rt.rtdName}
                />
              </FlexColumnSpaced>
            )
          )}

          <ListBindings
            values={result.expression.edBindings}
            types={result.expression.edTypeBindings}
            onBindingSelect={onBindingSelect}
            onFetchExpressionsForHashes={
              onFetchExpressionsForHashes
            }
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
    case 'ShowUnitTest':
      return (
        <FlexColumnSpaced>
          <Paragraph>Test created</Paragraph>
          <UnitTest unitTest={result.unitTest} />
          <ListBindings
            values={result.unitTest.utdBindings}
            types={{}}
            onBindingSelect={onBindingSelect}
            onFetchExpressionsForHashes={
              onFetchExpressionsForHashes
            }
          />
        </FlexColumnSpaced>
      )
    case 'EditorNew':
      return <FlexColumnSpaced />
  }
}
