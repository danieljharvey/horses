import { fold } from '@devexperts/remote-data-ts'
import { pipe } from 'fp-ts/lib/function'
import { useListExpressionTests } from '../../hooks/useListExpressionTests'
import { ExprHash, ProjectHash } from '../../types'
import { ListTests } from '../ListTests'
import { Paragraph } from '../View/Paragraph'

type ExpressionTestsProps = {
  projectHash: ProjectHash
  exprHash: ExprHash
}

export const ExpressionTests: React.FC<
  ExpressionTestsProps
> = ({ projectHash, exprHash }) => {
  const [expressionTests] = useListExpressionTests(
    projectHash,
    exprHash
  )
  return pipe(
    expressionTests,
    fold(
      () => <div />,
      () => <div />,
      (e) => <Paragraph>{e}</Paragraph>,
      (tests) => (
        <ListTests
          propertyTests={tests.propertyTests}
          unitTests={tests.unitTests}
        />
      )
    )
  )
}
