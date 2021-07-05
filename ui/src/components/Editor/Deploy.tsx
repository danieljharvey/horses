import * as React from 'react'
import { useDeployedExpression } from '../../hooks/useDeployedExpression'
import { ExprHash } from '../../types'
import { Button } from '../View/Button'
import { Paragraph } from '../View/Paragraph'
import { Code } from '../View/Code'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'

type Props = {
  exprHash: ExprHash
  title: string
}

export const Deploy: React.FC<Props> = ({
  exprHash,
  title,
}) => {
  const [deployState, deploy] = useDeployedExpression(
    exprHash
  )

  switch (deployState.type) {
    case 'Empty':
      return (
        <Button
          title="Deploy"
          onClick={deploy}
        >{`Deploy ${title}`}</Button>
      )
    case 'Fetching':
      return <p>Loading</p>
    case 'Failed':
      return (
        <FlexColumnSpaced>
          <Paragraph>Deployment failed!</Paragraph>
          <Code>{deployState.error}</Code>
          <Button onClick={deploy}>Retry</Button>
        </FlexColumnSpaced>
      )
    case 'HasDeployment':
      return (
        <FlexColumnSpaced>
          <Paragraph>Deployed successfully!</Paragraph>
          <Code>{deployState.url}</Code>
        </FlexColumnSpaced>
      )
  }
}
