import * as React from 'react'
import { getProjectGraph } from '../service/project'
import {
  initial,
  pending,
  RemoteData,
  failure,
  success,
} from '@devexperts/remote-data-ts'
import { GraphProjectResponse } from '../generated'
import { flow } from 'fp-ts/function'
import * as E from 'fp-ts/Either'
import { ExprHash } from '../types/'

// this is how we should do the screens from now on

type State = RemoteData<string, GraphProjectResponse>

export const useProjectGraph = (projectHash: ExprHash) => {
  const [graphData, setGraphData] = React.useState<State>(
    initial
  )

  React.useEffect(() => {
    setGraphData(pending)
    getProjectGraph(projectHash)().then(
      flow(
        E.fold<string, GraphProjectResponse, State>(
          failure,
          (a) => success(a)
        ),
        setGraphData
      )
    )
  }, [projectHash])

  return [graphData] as const
}
