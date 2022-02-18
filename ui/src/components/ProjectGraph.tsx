import * as React from 'react'

import './ProjectGraph.css'
import { Graphviz } from 'graphviz-react'
import { pipe } from 'fp-ts/function'
import { Panel } from './View/Panel'
import { Paragraph } from './View/Paragraph'
import { useProjectGraph } from '../hooks/useProjectGraph'
import * as O from 'fp-ts/Option'
import { ExprHash, GraphProjectResponse } from '../types'
import {
  RemoteData,
  toOption,
} from '@devexperts/remote-data-ts'
import { pushScreen } from '../reducer/view/actions'
import { findNameForExprHash } from '../reducer/project/helpers'
import { expressionGraphScreen } from '../reducer/view/screen'
import { useDispatch } from '../hooks/useDispatch'
import { useStoreRec } from '../hooks/useStore'
import { getProjectHash } from '../reducer/project/selectors'

type Props = {}

const findExpressionHash = ({
  nativeEvent,
}: React.MouseEvent<HTMLDivElement>): O.Option<ExprHash> => {
  const target = nativeEvent.target as any
  if (
    target?.localName === 'text' ||
    target?.localName === 'ellipse'
  ) {
    return O.fromNullable(
      target?.parentElement?.children[0].innerHTML
    )
  } else if (target?.localName === 'node') {
    return O.fromNullable(target?.children[0].innerHTML)
  }
  return O.none
}

// return either the expression data or the project data or nothing
const getGraphData = (
  projectGraphState: RemoteData<
    string,
    GraphProjectResponse
  >
): O.Option<string> =>
  pipe(
    toOption(projectGraphState),
    O.map((gp) => gp.gpGraphviz)
  )

export const ProjectGraph: React.FC<Props> = () => {
  const { projectHash, findName } = useStoreRec({
    projectHash: getProjectHash,
    findName: findNameForExprHash,
  })
  const dispatch = useDispatch()
  const [projectGraphState] = useProjectGraph(projectHash)

  const setSelectedExprHash = (
    hash: O.Option<ExprHash>
  ) => {
    if (O.isSome(hash)) {
      dispatch(
        pushScreen(
          expressionGraphScreen(
            pipe(
              findName(hash.value),
              O.fold(
                () => 'expression',
                (name) => name
              )
            ),
            hash.value
          )
        )
      )
    }
  }

  // try to use expr graph data if available, failing that, use project data,
  // failing that, show loading
  const useGraphData = getGraphData(projectGraphState)

  return (
    <>
      {pipe(
        useGraphData,
        O.fold(
          () => (
            <Panel>
              <Paragraph>🐴 Loading 🐴</Paragraph>
            </Panel>
          ),
          (graphData) => (
            <Panel
              onClick={(e) =>
                setSelectedExprHash(findExpressionHash(e))
              }
            >
              <Graphviz
                dot={graphData}
                className="graphviz"
                options={{
                  width: '100%',
                  zoom: true,
                }}
              />
            </Panel>
          )
        )
      )}
    </>
  )
}
