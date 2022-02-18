import * as React from 'react'
import { findNameForExprHash } from '../reducer/project/helpers'

import { findExpression } from '../reducer/project/helpers'
import './ProjectGraph.css'
import { Code } from '../components/View/Code'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'
import { Graphviz } from 'graphviz-react'
import { pipe } from 'fp-ts/function'
import { Panel } from './View/Panel'
import { Paragraph } from './View/Paragraph'
import * as O from 'fp-ts/Option'
import { ExprHash } from '../types'
import { pushScreen } from '../reducer/view/actions'
import { useDispatch } from '../hooks/useDispatch'
import { useStoreRec } from '../hooks/useStore'

type Props = {
  exprHash: ExprHash
  bindingName: string
}

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

export const ExpressionGraph: React.FC<Props> = ({
  exprHash,
  bindingName,
}) => {
  const dispatch = useDispatch()

  const { getGraphData, findName } = useStoreRec({
    getGraphData: findExpression,
    findName: findNameForExprHash,
  })

  const setSelectedExprHash = (
    hash: O.Option<ExprHash>
  ) => {
    if (O.isSome(hash)) {
      dispatch(
        pushScreen({
          type: 'expression-graph',
          exprHash: hash.value,
          bindingName: pipe(
            findName(hash.value),
            O.fold(
              () => 'expression',
              (name) => name
            )
          ),
        })
      )
    }
  }

  // try to use expr graph data if available, failing that, use project data,
  // failing that, show loading
  const useGraphData = getGraphData(exprHash)

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
          ({ expression }) => (
            <>
              <Panel
                flexGrow={2}
                onClick={(e) =>
                  setSelectedExprHash(findExpressionHash(e))
                }
              >
                <Graphviz
                  dot={expression.edGraphviz}
                  className="graphviz"
                  options={{
                    width: '100%',
                    zoom: true,
                  }}
                />
              </Panel>

              <Panel>
                <FlexColumnSpaced>
                  <Paragraph>{bindingName}</Paragraph>
                  <Code codeType="type">
                    {expression.edType}
                  </Code>
                  <Code>{expression.edPretty}</Code>
                </FlexColumnSpaced>
              </Panel>
            </>
          )
        )
      )}
    </>
  )
}
