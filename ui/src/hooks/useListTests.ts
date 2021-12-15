import * as React from 'react'
import { getProjectTests } from '../service/project'
import {
  initial,
  pending,
  RemoteData,
  failure,
  success,
} from '@devexperts/remote-data-ts'
import {
  ListTestsResponse,
  PropertyTestData,
  UnitTestData,
} from '../generated'
import { pipe } from 'fp-ts/function'
import * as E from 'fp-ts/Either'

// this is how we should do the screens from now on

type ListTests = {
  unitTests: UnitTestData[]
  propertyTests: PropertyTestData[]
}

export type ListTestsState = RemoteData<string, ListTests>

export const useListTests = (projectHash: string) => {
  const [listTestsState, setListTestsState] =
    React.useState<ListTestsState>(initial)

  React.useEffect(() => {
    setListTestsState(pending)

    getProjectTests(projectHash)().then((result) =>
      pipe(
        result,
        E.fold<string, ListTestsResponse, ListTestsState>(
          (e) => failure(e),
          (a) =>
            success({
              unitTests: a.ltTests.tdUnitTests,
              propertyTests: a.ltTests.tdPropertyTests,
            })
        ),
        setListTestsState
      )
    )
  }, [projectHash, setListTestsState])

  return [listTestsState] as const
}
