import * as React from 'react'
import { getTestsForExpression } from '../service/project'
import {
  initial,
  pending,
  RemoteData,
  failure,
  success,
} from '@devexperts/remote-data-ts'
import {
  ListTestsByExprHashResponse,
  PropertyTestData,
  UnitTestData,
} from '../generated'
import { pipe } from 'fp-ts/function'
import * as E from 'fp-ts/Either'
import { ExprHash } from '../types'

// this is how we should do the screens from now on

type ListTests = {
  unitTests: UnitTestData[]
  propertyTests: PropertyTestData[]
}

export type ListExpressionTestsState = RemoteData<
  string,
  ListTests
>

/**
 *
 * @param projectHash hash of project
 * @param exprHash hash of expression to fetch tests for
 * @returns current unit and property tests that use this expression
 */
export const useListExpressionTests = (
  projectHash: ExprHash,
  exprHash: ExprHash
) => {
  const [listTestsState, setListTestsState] =
    React.useState<ListExpressionTestsState>(initial)

  React.useEffect(() => {
    setListTestsState(pending)

    getTestsForExpression(projectHash, exprHash)().then(
      (result) =>
        pipe(
          result,
          E.fold<
            string,
            ListTestsByExprHashResponse,
            ListExpressionTestsState
          >(
            (e) => failure(e),
            ({
              ltbnTests: { tdPropertyTests, tdUnitTests },
            }) =>
              success({
                unitTests: tdUnitTests,
                propertyTests: tdPropertyTests,
              })
          ),
          setListTestsState
        )
    )
  }, [projectHash, exprHash, setListTestsState])

  return [listTestsState] as const
}
